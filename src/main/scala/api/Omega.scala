package api

trait Omega[T] {

  def complementary(omega: Set[T], a: Set[T]): Set[T] = omega -- a

  def union(a: Set[T], b: Set[T]): Set[T] = a ++ b

  def intersection(a: Set[T], b: Set[T]): Set[T] = a intersect b

}