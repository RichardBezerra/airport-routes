import Routes.Airport

import scala.collection.mutable
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

class DurationDistanceTrackingMap extends mutable.HashMap[Airport, Option[Int]] {
  def setDurationOfDepartureToZero(departure: Airport): Unit = {
    this.put(departure, Some(0))
  }

  def reduceDurationToArrivalIfRouteIsFaster(currentDuration: Option[Int], route: Routes.Route): Unit = {
    this (route.arrival) match {
      case Some(durationAtArrival) =>
        currentDuration match {
          case Some(duration) =>
            if (duration + route.durationHours < durationAtArrival) {
              this.put(route.arrival, Some(duration + route.durationHours))
            }
        }
      case None =>
        this.put(route.arrival, currentDuration.map(_ + route.durationHours))
    }
  }
}

object DurationDistanceTrackingMap {
  def apply(airports: Set[Airport]): DurationDistanceTrackingMap = {
    val durationDistanceTrackingMap = new DurationDistanceTrackingMap()
    durationDistanceTrackingMap.addAll(airports.map((_, None)))
  }
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
