package restrictions

import cats.Applicative
import akka.NotUsed
import akka.stream._
import akka.stream.scaladsl._
import restrictions.relation.ast.RowUpdate

package object akkaUtils {
  type UpdateSource[K, A] = Source[RowUpdate[K, A], NotUsed]

  implicit class ZipOps[A](left: Source[A, NotUsed]) {
    def amazingZipLatest[B](right: Source[B, NotUsed]): Source[(A,B), NotUsed] =
      Source.fromGraph(GraphDSL.create(left, right)(Keep.none) { implicit b =>
        (l, r) => {
          import GraphDSL.Implicits._
          val zip = b.add(new ZipLatest[A,B]())
          l ~> zip.in0
          r ~> zip.in1
          SourceShape(zip.out)
        }
      })
  }

  implicit val AkkaSourceApplicative: Applicative[Source[?, NotUsed]] = new Applicative[Source[?, NotUsed]] {
    override def pure[A](x: A): Source[A, NotUsed] = Source.single(x)

    override def ap[A, B](ff: Source[A => B, NotUsed])(fa: Source[A, NotUsed]): Source[B, NotUsed] =
      ff amazingZipLatest fa map { t => t._1(t._2) }

    override def map2[A, B, Z](fa: Source[A, NotUsed], fb: Source[B, NotUsed])(f: (A, B) => Z): Source[Z, NotUsed] =
      fa amazingZipLatest fb map { t => f(t._1, t._2) }
  }
}
