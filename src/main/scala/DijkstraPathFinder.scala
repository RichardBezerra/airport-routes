import Routes.Airport

import scala.util.{Success, Try}

trait DijkstraPathFinder {
  def dijkstra(graph: Map[Airport, Seq[Routes.Route]],
               arrival: Airport,
               numberOfAirports: Int): Try[Seq[Routes.Route]]

  def findShortestPath(graph: Map[Airport, Seq[Routes.Route]],
                       departure: Airport,
                       arrival: Airport,
                       numberOfAirports: Int,
                       dijkstra: Seq[Routes.Route]): Try[Seq[Routes.Route]]
}

object RouteDurationReverseOrdering extends Ordering[Routes.Route] {
  override def compare(routeA: Routes.Route,
                       routeB: Routes.Route): Int = -routeA.durationHours.compare(routeB.durationHours)
}

object LazyDijkstra extends DijkstraPathFinder {
  override def dijkstra(graph: Map[Airport, Seq[Routes.Route]],
                        departure: Airport,
                        numberOfAirports: Int): Try[Seq[Routes.Route]] = Success(Seq())

  override def findShortestPath(graph: Map[Airport, Seq[Routes.Route]],
                                departure: Airport,
                                arrival: Airport,
                                numberOfAirports: Int,
                                dijkstra: Seq[Routes.Route]): Try[Seq[Routes.Route]] = Success(Seq())
}
