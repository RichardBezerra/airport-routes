import Routes.Airport

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}
import scala.util.{Success, Try}

trait DijkstraPathFinder {
  def dijkstra(graph: Map[Airport, Seq[Routes.Route]],
               arrival: Airport,
               numberOfAirports: Int): Try[DurationDistanceTrackingMap]

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

  def reduceDurationToArrivalIfRouteIsFaster(currentDuration: Option[Int], route: Routes.Route): Option[Int] = {
    this (route.arrival) match {
      case Some(durationAtArrival) =>
        currentDuration match {
          case Some(duration) =>
            if (duration + route.durationHours < durationAtArrival) {
              this.put(route.arrival, Some(duration + route.durationHours))
              Some(duration + route.durationHours)
            } else {
              None
            }
        }
      case None =>
        this.put(route.arrival, currentDuration.map(_ + route.durationHours))
        currentDuration.map(_ + route.durationHours)
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
                        numberOfAirports: Int): Try[DurationDistanceTrackingMap] = {

    var visitedAirports: Seq[Airport] = Seq()

    val durationDistanceTrackingMap = DurationDistanceTrackingMap(Routes.groupAirports(Routes.providedRoutes))

    durationDistanceTrackingMap.setDurationOfDepartureToZero(departure)

    val routesPriorityQueue = mutable.PriorityQueue()(RouteDurationReverseOrdering)

    routesPriorityQueue.enqueue(Routes.Route(departure, departure, 0))

    while (routesPriorityQueue.nonEmpty) {
      val currentRoute = routesPriorityQueue.dequeue()
      visitedAirports = visitedAirports :+ currentRoute.departure

      breakable {
        if (durationDistanceTrackingMap(currentRoute.departure).exists(_ < currentRoute.durationHours)) {
          break
        }
      }

      graph(currentRoute.departure).foreach(route => {
        if (!visitedAirports.contains(route.arrival)) {
          val currentDurationAtDeparture = durationDistanceTrackingMap(currentRoute.departure)

          durationDistanceTrackingMap
            .reduceDurationToArrivalIfRouteIsFaster(currentDurationAtDeparture, route)
            // FIXME departure = route.arrival is hard to understand
            .map(newDuration => route.copy(departure = route.arrival, durationHours = newDuration))
            .foreach(routesPriorityQueue.enqueue(_))
        }
      })
    }

    Success(durationDistanceTrackingMap)
  }

  override def findShortestPath(graph: Map[Airport, Seq[Routes.Route]],
                                departure: Airport,
                                arrival: Airport,
                                numberOfAirports: Int,
                                dijkstra: Seq[Routes.Route]): Try[Seq[Routes.Route]] = Success(Seq())
}
