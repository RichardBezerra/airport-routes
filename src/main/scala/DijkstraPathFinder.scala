import Errors.{DepartureEqualToArrival, InvalidAirport, NoRoutesFound}
import Routes.Airport

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

trait DijkstraPathFinder {
  def dijkstra(graph: Map[Airport, Seq[Routes.Route]],
               departure: Airport,
               arrival: Airport,
               allAirports: Set[Airport]): Try[DurationDistanceTrackingMap]

  def findShortestPath(graph: Map[Airport, Seq[Routes.Route]],
                       departure: Airport,
                       arrival: Airport,
                       allAirports: Set[Airport],
                       dijkstra: DurationDistanceTrackingMap): Try[(Seq[Routes.Route], Int)]
}

object RouteDurationReverseOrdering extends Ordering[(Airport, TrackingPath)] {
  override def compare(x: (Airport, TrackingPath), y: (Airport, TrackingPath)): Int = {
    -x._2.totalDuration.compare(y._2.totalDuration)
  }
}

case class TrackingPath(isInitiated: Boolean, routes: Seq[Routes.Route]) {
  def totalDuration: Int = routes.map(_.durationHours).sum
}

object TrackingPath {
  def apply(): TrackingPath = new TrackingPath(false, Seq())

  def apply(routes: Seq[Routes.Route]): TrackingPath = new TrackingPath(true, routes)
}

class DurationDistanceTrackingMap extends mutable.HashMap[Airport, TrackingPath] {
  def setDurationOfDepartureToZero(departure: Airport): Unit = {
    this.put(departure, TrackingPath())
  }

  def reduceDurationToArrivalIfRouteIsFaster(currentTracking: TrackingPath, route: Routes.Route): Option[(Airport, TrackingPath)] = {
    val newDuration = this (route.arrival) match {
      case arrivalTracking @ TrackingPath(true, _) =>
        currentTracking match {
          case tracking @ TrackingPath(_, routes) =>
            if (tracking.totalDuration + route.durationHours < arrivalTracking.totalDuration) {
              TrackingPath(routes :+ route)
            } else {
              TrackingPath()
            }
        }
      case _ => TrackingPath(currentTracking.routes :+ route)
    }

    if (newDuration.isInitiated) {
      this.put(route.arrival, newDuration)
      Some((route.arrival, newDuration))
    } else {
      None
    }
  }
}

object DurationDistanceTrackingMap {
  def apply(airports: Set[Airport]): DurationDistanceTrackingMap = {
    val durationDistanceTrackingMap = new DurationDistanceTrackingMap()
    durationDistanceTrackingMap.addAll(airports.map((_, TrackingPath())))
  }
}

object LazyDijkstra extends DijkstraPathFinder {
  override def dijkstra(graph: Map[Airport, Seq[Routes.Route]],
                        departure: Airport,
                        arrival: Airport,
                        allAirports: Set[Airport]): Try[DurationDistanceTrackingMap] = {

    var visitedAirports: Seq[Airport] = Seq()

    val durationDistanceTrackingMap = DurationDistanceTrackingMap(allAirports)

    durationDistanceTrackingMap.setDurationOfDepartureToZero(departure)

    val routesPriorityQueue = mutable.PriorityQueue()(RouteDurationReverseOrdering)

    routesPriorityQueue.enqueue((departure, TrackingPath()))

    var arrivalFound = false

    while (routesPriorityQueue.nonEmpty && !arrivalFound) {
      val currentRoute = routesPriorityQueue.dequeue()
      visitedAirports = visitedAirports :+ currentRoute._1

      val isDurationToArrivalFaster =
        durationDistanceTrackingMap(currentRoute._1).totalDuration < currentRoute._2.totalDuration

      if (!isDurationToArrivalFaster) {

        graph(currentRoute._1).foreach(route => {
          if (!visitedAirports.contains(route.arrival)) {
            val currentDurationAtDeparture = durationDistanceTrackingMap(currentRoute._1)

            durationDistanceTrackingMap
              .reduceDurationToArrivalIfRouteIsFaster(currentDurationAtDeparture, route)
              .foreach(routesPriorityQueue.enqueue(_))
          }
        })

        arrivalFound = currentRoute._1 == arrival
      }
    }

    Success(durationDistanceTrackingMap)
  }

  override def findShortestPath(graph: Map[Airport, Seq[Routes.Route]],
                                departure: Airport,
                                arrival: Airport,
                                allAirports: Set[Airport],
                                dijkstra: DurationDistanceTrackingMap): Try[(Seq[Routes.Route], Int)] = {

    if (departure == arrival) {
      Failure(DepartureEqualToArrival)
    } else if (!graph.keySet.contains(departure) || !graph.keySet.contains(arrival)) {
      Failure(InvalidAirport)
    } else {
      val dijkstraResult = dijkstra(arrival)

      if (dijkstraResult.routes.isEmpty) {
        Failure(NoRoutesFound)
      } else {
        Success((dijkstraResult.routes, dijkstraResult.totalDuration))
      }
    }
  }
}
