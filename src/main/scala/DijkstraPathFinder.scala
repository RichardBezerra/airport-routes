import Errors.{DepartureEqualToArrival, InvalidAirport, NoRoutesFound}
import Routes.Airport

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

trait ShortestPathFinder {
  def findShortestPath(availableRoutes: Seq[Routes.Route],
                       departure: Airport,
                       arrival: Airport): DirectedCycleGraphFinder => Try[Seq[Routes.Route]] = { finder =>

    val allAirports = availableRoutes.flatMap(r => Set(r.departure, r.arrival)).toSet

    if (!allAirports.contains(departure) || !allAirports.contains(arrival)) Failure(InvalidAirport)
    else if (departure == arrival) Failure(DepartureEqualToArrival)
    else {
      val graph = Routes.buildGraph(availableRoutes)

      val hoursDistanceTracking = HoursTrack(allAirports)

      hoursDistanceTracking.setDurationOfDepartureToZero(departure)

      val routesPriorityQueue = mutable.PriorityQueue()(RouteDurationReverseOrdering)

      routesPriorityQueue.enqueue((departure, HoursTrackPathValue.notInitiated))

      finder.find(graph, allAirports, departure, arrival, routesPriorityQueue, hoursDistanceTracking)

      hoursDistanceTracking
        .get(arrival)
        .filter(_.routes.nonEmpty)
        .map(t => Success(t.routes))
        .getOrElse(Failure(NoRoutesFound))
    }
  }
}

trait DirectedCycleGraphFinder {

  def find(graph: Map[Airport, Seq[Routes.Route]],
           allAirports: Set[Airport],
           currentIterationAirport: Airport,
           arrival: Airport,
           priorityQueue: mutable.PriorityQueue[(Airport, HoursTrackPathValue)],
           durationDistanceTracking: HoursTrack)
}

object RouteDurationReverseOrdering extends Ordering[(Airport, HoursTrackPathValue)] {
  override def compare(x: (Airport, HoursTrackPathValue), y: (Airport, HoursTrackPathValue)): Int = {
    -x._2.totalDuration.compare(y._2.totalDuration)
  }
}

case class HoursTrackPathValue(routes: Seq[Routes.Route]) {
  val totalDuration: Int = routes.map(_.durationHours).sum
}

object HoursTrackPathValue {
  def apply(): HoursTrackPathValue = new HoursTrackPathValue(Seq())

  def apply(routes: Seq[Routes.Route]): HoursTrackPathValue = new HoursTrackPathValue(routes)

  val notInitiated: HoursTrackPathValue = new HoursTrackPathValue(Seq())
}

class HoursTrack extends mutable.HashMap[Airport, HoursTrackPathValue] {
  def setDurationOfDepartureToZero(departure: Airport): Unit = {
    this.put(departure, HoursTrackPathValue())
  }

  def reduceDurationToArrivalIfRouteIsFaster(currentTracking: HoursTrackPathValue,
                                             route: Routes.Route): Option[(Airport, HoursTrackPathValue)] = {
    this (route.arrival) match {
      case HoursTrackPathValue.notInitiated =>
        val firstTrackingPath = HoursTrackPathValue(currentTracking.routes :+ route)
        this.put(route.arrival, firstTrackingPath)
        Some((route.arrival, firstTrackingPath))

      case arrivalTracking : HoursTrackPathValue =>
        currentTracking match {
          case tracking @ HoursTrackPathValue(routes)
            if tracking.totalDuration + route.durationHours < arrivalTracking.totalDuration =>
            val fasterTrackingPath = HoursTrackPathValue(routes :+ route)
            this.put(route.arrival, fasterTrackingPath)
            Some((route.arrival, fasterTrackingPath))

          case _ => None
        }
    }
  }
}

object HoursTrack {
  def apply(airports: Set[Airport]): HoursTrack = {
    val durationDistanceTrackingMap = new HoursTrack()
    durationDistanceTrackingMap.addAll(airports.map((_, HoursTrackPathValue.notInitiated)))
  }
}

object LazyDijkstra extends DirectedCycleGraphFinder with ShortestPathFinder {

  def findShortestPath(graph: Map[Airport, Seq[Routes.Route]],
                                departure: Airport,
                                arrival: Airport,
                                allAirports: Set[Airport],
                                dijkstra: HoursTrack): Try[(Seq[Routes.Route], Int)] = {

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

  override def find(graph: Map[Airport, Seq[Routes.Route]],
                    allAirports: Set[Airport],
                    currentIterationAirport: Airport,
                    arrival: Airport,
                    routesPriorityQueue: mutable.PriorityQueue[(Airport, HoursTrackPathValue)],
                    durationDistanceTracking: HoursTrack): Unit = {

    val visitedAirports: mutable.HashMap[Airport, Boolean] = mutable.HashMap.from(allAirports.map((_, false)))

    var arrivalFound = false

    while (routesPriorityQueue.nonEmpty && !arrivalFound) {
      val currentRoute = routesPriorityQueue.dequeue()
      visitedAirports.put(currentRoute._1, true)

      val isDurationToArrivalFaster =
        durationDistanceTracking(currentRoute._1).totalDuration < currentRoute._2.totalDuration

      if (!isDurationToArrivalFaster) {

        graph(currentRoute._1).foreach(route => {
          if (!visitedAirports(route.arrival)) {
            val currentDurationAtDeparture = durationDistanceTracking(currentRoute._1)

            durationDistanceTracking
              .reduceDurationToArrivalIfRouteIsFaster(currentDurationAtDeparture, route)
              .foreach(routesPriorityQueue.enqueue(_))
          }
        })

        arrivalFound = currentRoute._1 == arrival
      }
    }
  }
}
