import Routes.Airport
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class RoutesTest extends AnyFlatSpec with Matchers {

  "Routes" should "build graph for providedRoutes" in {
    val graph: Map[Airport, Seq[Routes.Route]] = Routes.buildGraph(Routes.providedRoutes)

    // 9 departure airports
    graph should have size 9
  }

  it should "build degrees tracking sequence" in {
    val degreesTracking = Routes.buildDegreesTracking(null)

    degreesTracking should have size 10
    degreesTracking.find(_._1 == Routes.Airport("DUB")).get._2 should be(0)
    degreesTracking.find(_._1 == Routes.Airport("LAS")).get._2 should be(3)
  }
}
