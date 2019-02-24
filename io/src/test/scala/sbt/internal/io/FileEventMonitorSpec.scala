package sbt.internal.io

import java.nio.file.Paths
import java.util.concurrent.atomic.AtomicReference

import org.scalatest.{ FlatSpec, Matchers }
import sbt.internal.io.FileEvent.{ Creation, Deletion, Update }
import sbt.internal.io.FileEventMonitor.DeadlineSource

import scala.concurrent.duration._

class FileEventMonitorSpec extends FlatSpec with Matchers {
  import FileEventMonitorSpec._
  private[io] def antiEntropyMonitor[T <: SimpleFileAttributes](
      observable: Observable[FileEvent[T]],
      period: FiniteDuration,
      logger: WatchLogger,
      deadlineSource: DeadlineSource): FileEventMonitor[FileEvent[T]] =
    FileEventMonitor.antiEntropy(observable, period, logger, 50.millis, 10.minutes, deadlineSource)
  object TestAttributes {
    def apply(exists: Boolean = true,
              isDirectory: Boolean = false,
              isRegularFile: Boolean = false,
              isSymbolicLink: Boolean = false): SimpleFileAttributes =
      SimpleFileAttributes.get(exists, isDirectory, isRegularFile, isSymbolicLink)
  }
  class DeterministDeadlineSource extends DeadlineSource {
    val currentTime = new AtomicReference[Deadline](Deadline.now)
    override def now(): Deadline = currentTime.get()
    def increment(duration: FiniteDuration): Unit = {
      val current = currentTime.get()
      currentTime.set(current + duration)
    }
    def incrementAsync(duration: FiniteDuration): Unit = {
      new Thread("increment-deadline-source") {
        setDaemon(true)
        start()
        override def run(): Unit = {
          Thread.sleep(2)
          increment(duration)
        }
      }
      ()
    }
  }
  "anti-entropy" should "ignore redundant events" in {
    val observers = new Observers[Event]
    val antiEntropyPeriod = 20.millis
    val deadlineSource = new DeterministDeadlineSource
    val monitor = antiEntropyMonitor(observers, antiEntropyPeriod, NullWatchLogger, deadlineSource)
    val foo = Paths.get("foo")
    val startAttributes = TestAttributes(isRegularFile = true)
    val fooCreation = Creation(foo, startAttributes)
    observers.onNext(fooCreation)
    observers.onNext(Update(foo, startAttributes, startAttributes))
    val barAttributes = TestAttributes(isRegularFile = true)
    val bar = Paths.get("bar")
    val barCreation = Creation(bar, barAttributes)
    observers.onNext(barCreation)
    deadlineSource.incrementAsync(antiEntropyPeriod + 5.millis)
    monitor.poll(antiEntropyPeriod).toSet[Event] compare Set(fooCreation, barCreation)
    val wait = antiEntropyPeriod + 100.millis
    deadlineSource.incrementAsync(wait)
    monitor.poll(wait) shouldBe Nil
    val update = Update(foo, startAttributes, startAttributes)
    observers.onNext(update)
    monitor.poll(antiEntropyPeriod) compare Seq(update)
  }
  it should "not ignore new events" in {
    val observers = new Observers[Event]
    val antiEntropyPeriod = 20.millis
    val deadlineSource = new DeterministDeadlineSource
    val monitor =
      antiEntropyMonitor(observers, antiEntropyPeriod, NullWatchLogger, deadlineSource)
    val foo = Paths.get("foo")
    val fooAttributes = TestAttributes(isRegularFile = true)
    val fooCreation = Creation(foo, fooAttributes)
    val fooUpdate = Update(foo, fooAttributes, fooAttributes)
    observers.onNext(fooCreation)
    observers.onNext(Update(foo, fooAttributes, fooAttributes))
    val bar = Paths.get("bar")
    val barAttributes = TestAttributes(isRegularFile = true)
    val barCreation = Creation(bar, barAttributes)
    observers.onNext(barCreation)
    monitor.poll(antiEntropyPeriod).toSet[Event] compare Set(fooCreation, barCreation)
    new Thread("anti-entropy-test") {
      setDaemon(true)
      start()
      override def run(): Unit = {
        deadlineSource.increment(2 * antiEntropyPeriod)
        observers.onNext(Update(foo, fooAttributes, fooAttributes, deadlineSource.now()))
        deadlineSource.increment(10.seconds)
      }
    }
    // Ensure the timeout is long enough for the background thread to call onUpdate
    monitor.poll(5.seconds) compare Seq(fooUpdate)
  }
  it should "quarantine deletions" in {
    val observers = new Observers[Event]
    val antiEntropyPeriod = 40.millis
    val quarantinePeriod = antiEntropyPeriod / 2
    val deadlineSource = new DeterministDeadlineSource
    val monitor =
      FileEventMonitor.antiEntropy(observers,
                                   antiEntropyPeriod,
                                   NullWatchLogger,
                                   quarantinePeriod,
                                   10.minutes,
                                   deadlineSource)
    val foo = Paths.get("foo")
    val fooAttributes = TestAttributes(exists = false, isRegularFile = true)
    val fooDeletion = Deletion(foo, fooAttributes, deadlineSource.now())
    observers.onNext(fooDeletion)
    monitor.poll(0.millis) shouldBe Nil
    deadlineSource.incrementAsync(quarantinePeriod * 3)
    monitor.poll(quarantinePeriod * 2) shouldBe Seq(fooDeletion)
  }
  it should "immediately trigger for creations" in {
    val observers = new Observers[Event]
    val antiEntropyPeriod = 40.millis
    val quarantinePeriod = antiEntropyPeriod / 2
    val monitor =
      FileEventMonitor.antiEntropy(observers,
                                   antiEntropyPeriod,
                                   NullWatchLogger,
                                   quarantinePeriod,
                                   10.minutes)
    val foo = Paths.get("foo")
    val deletionAttributes = TestAttributes(exists = false, isRegularFile = true)
    val fooDeletion = Deletion(foo, deletionAttributes)
    val creationAttributes = TestAttributes(isRegularFile = true)
    val fooCreation = Creation(foo, creationAttributes)
    observers.onNext(fooDeletion)
    observers.onNext(fooCreation)

    monitor.poll(0.millis) compare Seq(Update(foo, deletionAttributes, creationAttributes))
  }
}
object FileEventMonitorSpec extends Matchers {
  private type Event = FileEvent[SimpleFileAttributes]
  private val now = Deadline.now
  private implicit class FileEventOps(val fileEvent: Event) extends AnyVal {
    def stripOccurredAt: FileEvent[SimpleFileAttributes] = fileEvent match {
      case Creation(path, attributes, _)         => Creation(path, attributes, now)
      case Deletion(path, attributes, _)         => Deletion(path, attributes, now)
      case Update(path, previous, attributes, _) => Update(path, previous, attributes, now)
    }
  }
  private implicit class TraversableEventOps[T <: Traversable[Event]](val t: T) extends AnyVal {
    def compare[S <: Traversable[Event]](that: S): Unit = {
      val left = t.map(_.stripOccurredAt)
      val right = that.map(_.stripOccurredAt)
      assert(left == right)
      ()
    }
  }
}
