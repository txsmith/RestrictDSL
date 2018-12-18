package restrictions.domain.external

sealed trait RowUpdate[K,+A] {
  def key: K
  def map[B](f: A => B): RowUpdate[K, B]
  def mapWithKey[L,B](f: A => B, g: K => L): RowUpdate[L, B]
  def toEither: Either[K, (K, A)]
}
case class Update[K,A](key: K, value: A) extends RowUpdate[K,A] {
  override def map[B](f: A => B) = Update(key, f(value))
  override def toEither = Right((key, value))
  override def mapWithKey[L, B](f: A => B, g: K => L) = Update(g(key), f(value))
}
case class Delete[K,A](key: K) extends RowUpdate[K,A] {
  override def map[B](f: A => B) = Delete(key)
  override def toEither = Left(key)
  override def mapWithKey[L, B](f: A => B, g: K => L) = Delete(g(key))
}
object RowUpdate {
  def apply[K,A](key: K, value: A): restrictions.domain.external.RowUpdate[K, A] =
    Update(key, value)

  def delete[K, A](key: K): restrictions.domain.external.RowUpdate[K, A] =
    Delete(key)
}
