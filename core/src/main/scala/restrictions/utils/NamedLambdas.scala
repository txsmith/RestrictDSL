package restrictions.utils

import sourcecode.{FullName, Line}

import scala.language.implicitConversions

trait NamedLambdas {
  def id[A]: NamedLambda[A, A] = NamedLambda(a => a, "id")
  def fst[A,B]: NamedLambda[(A,B), A] = NamedLambda((t: (A,B)) => t._1, "fst")
  def snd[A,B]: NamedLambda[(A,B), B] = NamedLambda((t: (A,B)) => t._2, "snd")

  def fullName[A,B](f: A => B)(implicit sourceName: FullName, sourceLoc: Line): NamedLambda[A,B] =
    NamedLambda(f, sourceName.value + ":" + sourceLoc.value)

  def name[A,B](f: A => B)(implicit sourceName: sourcecode.Name): NamedLambda[A,B] =
    NamedLambda(f, sourceName.value)


  trait NamedLambda[-A,+B] extends (A => B) {
    val fnName: String
    override def toString(): String = fnName

    override def compose[Z](g: Z => A): Z => B = g match {
      case _: NamedLambda[Z,A] =>  NamedLambda((z: Z) => this.apply(g(z)), s"$g ∘ ${this.fnName}")
      case _ => NamedLambda((z: Z) => this.apply(g(z)), s"λ($g) ∘ ${this.fnName}")
    }

    override def andThen[Z](g: B => Z): A => Z = g match {
      case _: NamedLambda[B,Z] =>  NamedLambda((a: A) => g(this.apply(a)), s"${this.fnName} ∘ $g")
      case _ => NamedLambda((a: A) => g(this.apply(a)), s"${this.fnName} ∘ λ($g)")
    }

    def fullName(implicit sourceName: FullName, sourceLoc: Line): NamedLambda[A,B] =
      NamedLambda(this, sourceName.value + ":" + sourceLoc.value)

    def name(implicit sourceName: sourcecode.Name): NamedLambda[A,B] =
      NamedLambda(this, sourceName.value)

    def name(s: String): NamedLambda[A,B] =
      NamedLambda(this, s)
  }

  object NamedLambda {
    def apply[A,B](f: A => B, functionName: String): NamedLambda[A,B] = new NamedLambda[A,B] {
      override val fnName = functionName
      override def apply(v1: A) = f(v1)
    }
  }

  implicit def ToNamedLambda[A,B](f: A => B)(implicit sourceName: FullName, sourceLoc: Line): NamedLambda[A,B] =
    f match {
      case g: NamedLambda[A,B] => g
      case _ => fullName(f)
    }
}
