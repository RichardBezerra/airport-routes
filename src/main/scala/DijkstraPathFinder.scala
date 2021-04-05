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

      hoursDistanceTracking.setDurationToZero(departure)

      val routesPriorityQueue = mutable.PriorityQueue()(RouteDurationReverseOrdering)

      routesPriorityQueue.enqueue((departure, HoursTrackPathValue.notInitiated))

      finder.fillHoursTrack(graph, allAirports, departure, arrival, hoursDistanceTracking)

      hoursDistanceTracking
        .get(arrival)
        .filter(_.routes.nonEmpty)
        .map(t => Success(t.routes))
        .getOrElse(Failure(NoRoutesFound))
    }
  }
}

trait DirectedCycleGraphFinder {

  def fillHoursTrack(graph: Map[Airport, Seq[Routes.Route]], allAirports: Set[Airport], currentIterationAirport: Airport, arrival: Airport, hoursTrack: HoursTrack): Unit
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
  def setDurationToZero(airport: Airport): Unit = {
    this.put(airport, HoursTrackPathValue())
  }

  def overridePathToArrivalIfRouteIsFaster(currentTracking: HoursTrackPathValue,
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

object LazyDijkstra extends ShortestPathFinder with DirectedCycleGraphFinder {

  override def fillHoursTrack(graph: Map[Airport, Seq[Routes.Route]], allAirports: Set[Airport], departure: Airport, arrival: Airport, hoursTrack: HoursTrack): Unit = {

    val routesPriorityQueue = mutable.PriorityQueue()(RouteDurationReverseOrdering)

    routesPriorityQueue.enqueue((departure, HoursTrackPathValue.notInitiated))

    val visitedAirports: mutable.HashMap[Airport, Boolean] = mutable.HashMap.from(allAirports.map((_, false)))

    var arrivalFound = false

    while (routesPriorityQueue.nonEmpty && !arrivalFound) {
      val currentRoute = routesPriorityQueue.dequeue()
      visitedAirports.put(currentRoute._1, true)

      val isDurationToArrivalFaster =
        hoursTrack(currentRoute._1).totalDuration < currentRoute._2.totalDuration

      if (!isDurationToArrivalFaster) {

        graph(currentRoute._1).foreach(route => {
          if (!visitedAirports(route.arrival)) {
            val currentDurationAtDeparture = hoursTrack(currentRoute._1)

            hoursTrack
              .overridePathToArrivalIfRouteIsFaster(currentDurationAtDeparture, route)
              .foreach(routesPriorityQueue.enqueue(_))
          }
        })

        arrivalFound = currentRoute._1 == arrival
      }
    }
  }
}
