package com.airplanerouteschallenge

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
 * Finder that aims to search for shortest paths in graphs that are directed and cyclic.
 * In order to that it takes a Dijkstra implementation.
 */
trait DirectedCycleGraphFinder extends ShortestPathFinder {

  override def findShortestPath(availableRoutes: Seq[Route],
                                departure: Airport,
                                arrival: Airport): Try[Seq[Route]] = {

    findShortestPathWith(availableRoutes, departure, arrival)(LazyDijkstra)
  }

  /**
   * Finds the shortest path from an origin airport to a destination airport among informed routes.
   * @param availableRoutes all possibles routes.
   * @param departure origin of the path.
   * @param arrival destination of the path.
   * @return sequence of routes that goes from origin (first item of the sequence)
   *         to destination (last item of the sequence).
   *         It takes a Dijsktra implementation for doing that.
   */
  def findShortestPathWith(availableRoutes: Seq[Route],
                           departure: Airport,
                           arrival: Airport): Dijkstra => Try[Seq[Route]] = { finder =>

    val allAirports = airports(availableRoutes)

    if (!allAirports.contains(departure) || !allAirports.contains(arrival)) Failure(InvalidAirport(departure.iataCode))
    else if (departure == arrival) Failure(DepartureEqualToArrival(departure.iataCode))
    else {
      val graph = buildGraph(availableRoutes)

      val hoursDistanceTracking = finder.generateHoursTrack(graph, allAirports, departure, arrival)

      hoursDistanceTracking
        .get(arrival)
        .filter(_.routes.nonEmpty)
        .map(t => Success(t.routes))
        .getOrElse(Failure(NoRoutesFound))
    }
  }

}

object DirectedCycleGraphFinder extends DirectedCycleGraphFinder

/**
 * Contract for Dijkstra algorithm.
 */
trait Dijkstra {

  /**
   * Queue that is used for keeping tracking of what airports are the next to be visited
   * while fillHoursTrack iterates over the graph.
   * @return priority queue that order items in reverse order.
   */
  def priorityQueue: mutable.PriorityQueue[(Airport, HoursTrackPathValue)]

  /**
   * Fill hoursTrack while searches for shortest path from departure to arrival.
   * This is not a pure function. It keeps changing hoursTrack for each call.
   *
   * @param graph that is traversed while searching for the shortest path.
   * @param allAirports list of all airports in the graph.
   * @param departure origin of the path.
   * @param arrival destination of the path.
   * @return map that holds the shortest path from departure to other airports that gets
   *         passed through until destination is found.
   */
  def generateHoursTrack(graph: Map[Airport, Seq[Route]],
                         allAirports: Set[Airport],
                         departure: Airport,
                         arrival: Airport): HoursTrack
}

/**
 * Its a lazy implementation because it might add destination airport more than once in the priority queue.
 */
object LazyDijkstra extends Dijkstra {

  lazy val priorityQueue: mutable.PriorityQueue[(Airport, HoursTrackPathValue)] = {
    val reverseHoursOrdering: Ordering[(Airport, HoursTrackPathValue)] = (x: (Airport, HoursTrackPathValue),
                                                                          y: (Airport, HoursTrackPathValue)) => {
      -x._2.totalDuration.compare(y._2.totalDuration)
    }

    mutable.PriorityQueue()(reverseHoursOrdering)
  }

  override def generateHoursTrack(graph: Map[Airport, Seq[Route]],
                                  allAirports: Set[Airport],
                                  departure: Airport,
                                  arrival: Airport): HoursTrack = {

    val hoursDistanceTrack: HoursTrack = createHoursTrack(allAirports, departure)

    priorityQueue.enqueue((departure, HoursTrackPathValue.notInitiated))

    val visitedAirports = mutable.HashMap.from(allAirports.map((_, false)))

    var arrivalFound = false

    while (priorityQueue.nonEmpty && !arrivalFound) {
      val currentRoute = priorityQueue.dequeue()
      visitedAirports.put(currentRoute._1, true)

      val isDurationToArrivalFaster =
        hoursDistanceTrack(currentRoute._1).totalDuration < currentRoute._2.totalDuration

      if (!isDurationToArrivalFaster) {

        graph(currentRoute._1).foreach(route => {
          if (!visitedAirports(route.arrival)) {
            val currentDurationAtDeparture = hoursDistanceTrack(currentRoute._1)

            hoursDistanceTrack
              .overridePathToArrivalIfRouteIsFaster(currentDurationAtDeparture, route)
              .foreach(priorityQueue.enqueue(_))
          }
        })

        arrivalFound = currentRoute._1 == arrival
      }
    }

    hoursDistanceTrack
  }

  private def createHoursTrack(allAirports: Set[Airport], departure: Airport) = {
    val hoursDistanceTracking = HoursTrack(allAirports)
    hoursDistanceTracking.setDurationToZero(departure)
    hoursDistanceTracking
  }
}

/**
 * Map that holds the path from a departure (the one that is initialized with HoursTrackPathValue as zero)
 * to some other airports while the shortest path is searched for.
 * Key is an Airport.
 * Value is a HoursTrackPathValue that holds a sequence of routes along with the sum of hours across all its routes.
 */
class HoursTrack extends mutable.HashMap[Airport, HoursTrackPathValue] {
  def setDurationToZero(airport: Airport): Unit = {
    this.put(airport, HoursTrackPathValue())
  }

  def overridePathToArrivalIfRouteIsFaster(currentTracking: HoursTrackPathValue,
                                           route: Route): Option[(Airport, HoursTrackPathValue)] = {
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
    new HoursTrack().addAll(airports.map((_, HoursTrackPathValue.notInitiated)))
  }
}

case class HoursTrackPathValue(routes: Seq[Route]) {
  val totalDuration: Int = routes.map(_.durationHours).sum
}

object HoursTrackPathValue {
  def apply(): HoursTrackPathValue = new HoursTrackPathValue(Seq())

  def apply(routes: Seq[Route]): HoursTrackPathValue = new HoursTrackPathValue(routes)

  val notInitiated: HoursTrackPathValue = new HoursTrackPathValue(Seq())
}

