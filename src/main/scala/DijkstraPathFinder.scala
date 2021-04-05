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

      val hoursDistanceTracking = DurationDistanceTracking(allAirports)

      hoursDistanceTracking.setDurationOfDepartureToZero(departure)

      val routesPriorityQueue = mutable.PriorityQueue()(RouteDurationReverseOrdering)

      routesPriorityQueue.enqueue((departure, TrackingPath.notInitiated))

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
           priorityQueue: mutable.PriorityQueue[(Airport, TrackingPath)],
           durationDistanceTracking: DurationDistanceTracking)
}

trait DijkstraPathFinder {
  def dijkstra(graph: Map[Airport, Seq[Routes.Route]],
               departure: Airport,
               arrival: Airport,
               allAirports: Set[Airport]): Try[DurationDistanceTracking]

  def findShortestPath(graph: Map[Airport, Seq[Routes.Route]],
                       departure: Airport,
                       arrival: Airport,
                       allAirports: Set[Airport],
                       dijkstra: DurationDistanceTracking): Try[(Seq[Routes.Route], Int)]
}

object RouteDurationReverseOrdering extends Ordering[(Airport, TrackingPath)] {
  override def compare(x: (Airport, TrackingPath), y: (Airport, TrackingPath)): Int = {
    -x._2.totalDuration.compare(y._2.totalDuration)
  }
}

case class TrackingPath(routes: Seq[Routes.Route]) {
  val totalDuration: Int = routes.map(_.durationHours).sum
}

object TrackingPath {
  def apply(): TrackingPath = new TrackingPath(Seq())

  def apply(routes: Seq[Routes.Route]): TrackingPath = new TrackingPath(routes)

  val notInitiated: TrackingPath = new TrackingPath(Seq())
}

class DurationDistanceTracking extends mutable.HashMap[Airport, TrackingPath] {
  def setDurationOfDepartureToZero(departure: Airport): Unit = {
    this.put(departure, TrackingPath())
  }

  def reduceDurationToArrivalIfRouteIsFaster(currentTracking: TrackingPath,
                                             route: Routes.Route): Option[(Airport, TrackingPath)] = {
    this (route.arrival) match {
      case TrackingPath.notInitiated =>
        val firstTrackingPath = TrackingPath(currentTracking.routes :+ route)
        this.put(route.arrival, firstTrackingPath)
        Some((route.arrival, firstTrackingPath))

      case arrivalTracking : TrackingPath =>
        currentTracking match {
          case tracking @ TrackingPath(routes)
            if tracking.totalDuration + route.durationHours < arrivalTracking.totalDuration =>
            val fasterTrackingPath = TrackingPath(routes :+ route)
            this.put(route.arrival, fasterTrackingPath)
            Some((route.arrival, fasterTrackingPath))

          case _ => None
        }
    }
  }
}

object DurationDistanceTracking {
  def apply(airports: Set[Airport]): DurationDistanceTracking = {
    val durationDistanceTrackingMap = new DurationDistanceTracking()
    durationDistanceTrackingMap.addAll(airports.map((_, TrackingPath.notInitiated)))
  }
}

object LazyDijkstra extends DijkstraPathFinder with DirectedCycleGraphFinder with ShortestPathFinder {
  override def dijkstra(graph: Map[Airport, Seq[Routes.Route]],
                        departure: Airport,
                        arrival: Airport,
                        allAirports: Set[Airport]): Try[DurationDistanceTracking] = {

    val visitedAirports: mutable.HashMap[Airport, Boolean] = mutable.HashMap.from(allAirports.map((_, false)))

    val durationDistanceTrackingMap = DurationDistanceTracking(allAirports)

    durationDistanceTrackingMap.setDurationOfDepartureToZero(departure)

    val routesPriorityQueue = mutable.PriorityQueue()(RouteDurationReverseOrdering)

    routesPriorityQueue.enqueue((departure, TrackingPath.notInitiated))

    var arrivalFound = false

    while (routesPriorityQueue.nonEmpty && !arrivalFound) {
      val currentRoute = routesPriorityQueue.dequeue()
      visitedAirports.put(currentRoute._1, true)

      val isDurationToArrivalFaster =
        durationDistanceTrackingMap(currentRoute._1).totalDuration < currentRoute._2.totalDuration

      if (!isDurationToArrivalFaster) {

        graph(currentRoute._1).foreach(route => {
          if (!visitedAirports(route.arrival)) {
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
                                dijkstra: DurationDistanceTracking): Try[(Seq[Routes.Route], Int)] = {

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
                    routesPriorityQueue: mutable.PriorityQueue[(Airport, TrackingPath)],
                    durationDistanceTracking: DurationDistanceTracking): Unit = {

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
