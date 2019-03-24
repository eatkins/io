package sbt.io

trait FileAttributes {
  def hash: Option[String]
  def lastModified: Option[Long]
  def isRegularFile: Boolean
  def isDirectory: Boolean
  def isSymbolicLink: Boolean
}
