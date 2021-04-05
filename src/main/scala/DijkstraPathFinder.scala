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
                        numberOfAirports: Int): Try[DurationDistanceTrackingMap] = {

    var visitedAirports: Seq[Airport] = Seq()

    val durationDistanceTrackingMap = DurationDistanceTrackingMap(Routes.groupAirports(Routes.providedRoutes))

    durationDistanceTrackingMap.setDurationOfDepartureToZero(departure)

    val routesPriorityQueue = mutable.PriorityQueue()(RouteDurationReverseOrdering)

    routesPriorityQueue.enqueue((departure, TrackingPath()))

    while (routesPriorityQueue.nonEmpty) {
      val currentRoute = routesPriorityQueue.dequeue()
      visitedAirports = visitedAirports :+ currentRoute._1

      breakable {
        if (durationDistanceTrackingMap(currentRoute._1).totalDuration < currentRoute._2.totalDuration) {
          break
        }
      }

      graph(currentRoute._1).foreach(route => {
        if (!visitedAirports.contains(route.arrival)) {
          val currentDurationAtDeparture = durationDistanceTrackingMap(currentRoute._1)

          durationDistanceTrackingMap
            .reduceDurationToArrivalIfRouteIsFaster(currentDurationAtDeparture, route)
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
