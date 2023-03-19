package com.phasmidsoftware.examples.crime

import cats.effect.IO
import com.phasmidsoftware.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.table.{Analysis, HeadedTable, RawTable, Table}
import com.phasmidsoftware.util.EvaluateIO.matchIO
import com.phasmidsoftware.util.FP.resource
import com.phasmidsoftware.util.IOUsing
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import scala.io.Source

class CrimeFuncSpec extends AnyFlatSpec with Matchers {

  behavior of "Crime"

  /**
   * The following file is ignored for git purposes:
   * You need to download and extract it from here:
   * [[https://www.kaggle.com/datasets/marshuu/crimes-in-uk-2023/download]]
   */
  val crimeFile = "2023-01-metropolitan-street.csv"

  it should "be ingested and analyzed as a RawTable" in {

    // Set up the source
    val sy: IO[Source] = IO.fromTry(for (u <- resource[CrimeFuncSpec](crimeFile)) yield Source.fromURL(u))

    val fraction = 4
    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(fraction))

    // Create the table
    val wsty: IO[RawTable] = parser.parse(sy)

// CONSIDER how is it that this test runs in around 157 seconds yet the timeout is set to 30 seconds?
    matchIO(wsty, Timeout(Span(30, Seconds))) {
      case t@HeadedTable(r, _) =>
        val analysis = Analysis(t)
        println(s"Crime: $analysis")
        analysis.rows shouldBe 87211 / fraction +- 2000
        r take 10 foreach println
        succeed
    }
  }

    it should "be ingested as a Table[Crime]" in {

    import CrimeParser._

    // Create the table
    val wsty: IO[Table[Crime]] = Table.parseResource(crimeFile, classOf[CrimeFuncSpec])

    matchIO(wsty, Timeout(Span(60, Seconds))) {
      case t@HeadedTable(r, _) =>
        t.size shouldBe 87211
        r take 100 foreach println
        succeed
    }
  }

  it should "be ingested and written out properly to CSV" in {
    import CrimeParser._
    import CrimeRenderer._

    val mti: IO[Table[Crime]] = IOUsing(Source.fromURL(classOf[Crime].getResource(crimeFile)))(x => Table.parseSource(x))

    val wi: IO[String] = mti flatMap (_.toCSV)
    matchIO(wi, Timeout(Span(60, Seconds))) {
      case w =>
        w.substring(0, 100) shouldBe ",crimeID,month,reportedBy,fallsWithin,crimeLocation.longitude,crimeLocation.latitude,crimeLocation.l"
    }
  }

  it should "be ingested and written out in brief to CSV" in {
    import CrimeLocationRenderer._
    import CrimeParser._

    val cti: IO[Table[Crime]] = IOUsing(Source.fromURL(classOf[Crime].getResource(crimeFile)))(x => Table.parseSource(x))

    val wi: IO[String] = for {
      ct <- cti
      lt <- IO(ct.mapOptional(m => m.brief).filter(m => m.crimeID.isDefined))
      st <- IO(lt.sort.slice(150, 170))
      w <- st.toCSV
    } yield w

    matchIO(wi, Timeout(Span(60, Seconds))) {
      case w =>
        w shouldBe
                """crimeID,longitude,latitude
                  |85b4a97f2b802503658333bff2b1cbb6a85179b3d720b78692feebcf2d63dc,-0.027238,51.474771
                  |863604f90d65cdcf5ccb7d864dae9580d8c01be1a73f4415f1254f5dbb493b,-0.452489,51.469799
                  |86c3452bc289b73d2d5111165c63242b1e068647ec58fbc88dd8ee6d2f545e,0.121723,51.55056
                  |872b7ca64fa7582d3f165bb11af0524ddd3ff24afdf7a90c58662fb9b29049,-0.224735,51.492891
                  |87816b5ceefd0bc30a88073ba0f84d9c83279e66892fdb90a31d648b042c00,0.031268,51.477963
                  |87f6ca3cad6a4bd66cc395776ec092056ae4ef9d4205eeb658b1d6a484f279,-0.230917,51.546408
                  |87f6ca3cad6a4bd66cc395776ec092056ae4ef9d4205eeb658b1d6a484f279,-0.230917,51.546408
                  |882ad36f02eb8ed1fdc846f8deeff9f0a0fcfa7ec4de367347e53ba930e6aa,0.051967,51.538681
                  |886394cfdc3700537b6ef7e75baec294c57c6eca203bfc824c7b25f4d1510d,-0.084944,51.484289
                  |886394cfdc3700537b6ef7e75baec294c57c6eca203bfc824c7b25f4d1510d,-0.084944,51.484289
                  |8904d5e3c878c4597d36cf612b0a4dca7e092fab224f22367c0282949e1d6d,-0.286526,51.466599
                  |89f7f4c1b6f03ac1a3c36c5ba9f40673a35bcaed46c49e790b9abff529d0fc,-0.062929,51.559519
                  |8ab124ca3d2f07f7b4c910c57992a44d918ecd21ae7755a85e407b7b78e122,0.057263,51.606213
                  |8ad32137e8bae5a0004dcc76e20c818f12dedce7d03e3df0d4e3b8e7b93d13,0.112912,51.488012
                  |8cb06b69ac2aebee7e0340280231a72d5bcfb37d254b7b6a80356f0777ba1f,-0.057831,51.508842
                  |8e0b7353d6eff0467607699256e7f68ada36eb7ffcaa82049d299d97b8622d,-0.077877,51.524577
                  |8f9321afab6802cd1b6b46ece05c7cd0cb53e1f2bb073cdfc3aeeeb414cbf1,-0.038254,51.437501
                  |8fa8b9fd0e95a234069ae923627a4efc20c6f1c921aa738b0007c634e851a0,-0.199476,51.543124
                  |902a35564fa1a7a9b2648173055d65d996453d6f48a848a2c5d14b03f71fdd,-0.071621,51.572656
                  |929962fbc0f72c0c1449501b56d6fec7905f0cffe85752d6c63acc56bd21a0,-0.115433,51.387509
                  |""".stripMargin
    }
  }

}
