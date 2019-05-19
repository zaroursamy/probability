package api

trait Measurable[T] extends Omega[T] {
  def mu(t: T*): Double
}

object Measurable {

  def apply[T](measure: T ⇒ Double): Measurable[T] = new Measurable[T] {
    override def mu(t: T*): Double = t.distinct.map(measure).sum
  }

  def mu[T](ts: T*)(implicit measurable: Measurable[T]): Double = measurable.mu(ts: _*)

}
