import Routes.Airport
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class RoutesTest extends AnyFlatSpec with Matchers {

  "Routes" should "build graph for providedRoutes" in {
    val graph: Seq[(Airport, Seq[Routes.Route])] = Routes.buildGraph(Routes.providedRoutes)

    // 9 departure airports
    graph should have size 9
  }
}
