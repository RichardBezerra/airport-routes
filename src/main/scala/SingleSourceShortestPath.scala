import Routes.Airport

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object SingleSourceShortestPath {

  case object InvalidDagCyclesFound extends Throwable
  case object DepartureEqualToArrival extends Throwable
  case object NoRoutesFound extends Throwable
  case object InvalidAirport extends Throwable

  /**
   * Creates a topological order representation of the graph that is generated from the provided routes.
   * @param routes that will be transformed to a graph.
   * @return a sequence of airports that represents the topological order
   *         of the graph if the graph is a Directed Acyclic Graph (DAG).
   *         Otherwise, a failure if graph is not a DAG.
   */
  def createTopologicalOrder(routes: Seq[Routes.Route]): Try[Seq[Airport]] = {
    val graph = Routes.buildGraph(routes)

    // Map[Airport, amount of routes that have this airport as arrival].
    // It could be the graph instead of the routes.
    val initialAmountArrivalConnections: Map[Airport, Int] = Routes.groupAirports(routes)
      .map(airport => (airport, routes.count(_.arrival == airport))).toMap

    // A mutable Map[Airport, amount of routes that have this airport as arrival]
    // used to track the the decrement of connections while topological order is being created.
    val mutableAmountArrivalConnections: mutable.Map[Airport, Int] = mutable.Map.from(initialAmountArrivalConnections)

    // Initiate with airports that are not arrival of any route
    val airportsToProcess: mutable.Queue[Airport] =
      mutable.Queue.from(initialAmountArrivalConnections.filter(_._2 == 0).keys)

    var visitedAirportsCounter: Int = 0

    var topologicalOrder: Seq[Airport] = Seq()

    while (airportsToProcess.nonEmpty) {
      val airport = airportsToProcess.dequeue()
      topologicalOrder = topologicalOrder :+ airport
      visitedAirportsCounter = visitedAirportsCounter + 1

      graph.get(airport).foreach(_.foreach { route =>
        // decrease number of routes that are connected current airport
        // since it was 'removed' from the list of nodes that still needs to be processed.
        mutableAmountArrivalConnections.put(route.arrival, mutableAmountArrivalConnections(route.arrival) - 1)

        // add airport to process list when it no longer has routes to it.
        if (mutableAmountArrivalConnections(route.arrival) == 0) {
          airportsToProcess.enqueue(route.arrival)
        }
      })
    }

    if (visitedAirportsCounter == graph.size) {
      Success(topologicalOrder)
    } else {
      Failure(InvalidDagCyclesFound)
    }
  }



  /**
   * Creates the Single Source Shortest Path (SSP) from the topological order provided.
   * @param graph that will be used to check the hours in order to build the shortest duration
   *              from the departure to all arrival airports.
   * @param topologicalOrder which sequence will be used to generate the SSSP.
   * @param departure airport. The 'source' of SSSP.
   * @return sequence of tuple as (Airport, Hours distant from the source)
   *         in same order as topological order.
   */
  def createSSSP(graph: Map[Airport, Seq[Routes.Route]],
                 topologicalOrder: Seq[Airport],
                 departure: Airport): Seq[(Airport, Option[Seq[Routes.Route]])] = {

    // initiate an array to track the hours from source to each airport
    val hoursTracking: mutable.ArraySeq[(Airport, Option[Seq[Routes.Route]])] =
      mutable.ArraySeq.from(topologicalOrder.map((_, None)))

    hoursTracking(topologicalOrder.indexOf(departure)) = (departure, Some(Seq()))

    for (airport <- topologicalOrder) {

      val airportIndexAtTopOrder = topologicalOrder.indexOf(airport)

      graph(airport).foreach { route =>

        hoursTracking(airportIndexAtTopOrder)._2.foreach { routesOfCurrentAirport =>
          val durationAtCurrentAirport = routesOfCurrentAirport.map(_.durationHours).sum

          val arrivalAirportIndexAtTopOrder = topologicalOrder.indexOf(route.arrival)

          val newDuration = durationAtCurrentAirport + route.durationHours

          // update each airport with minimum duration from the source
          hoursTracking(arrivalAirportIndexAtTopOrder)._2 match {
            case Some(routesAtArrival) =>
              val durationAtArrival = routesAtArrival.map(_.durationHours).sum
              if (newDuration < durationAtArrival) {
                hoursTracking(arrivalAirportIndexAtTopOrder) = (route.arrival,
                  hoursTracking(airportIndexAtTopOrder)._2.map(_ :+ route))
              }
            case None =>
              hoursTracking(arrivalAirportIndexAtTopOrder) = (route.arrival,
                hoursTracking(airportIndexAtTopOrder)._2.map(_ :+ route))
          }
        }
      }
    }

    hoursTracking.toSeq
  }

  /**
   * Finds the shortest path among informed routes between the departure and the arrival airports.
   * @param departure one of the valid airports from 'Routes.providedRoutes' list.
   * @param arrival one of the valid airports from 'Routes.providedRoutes' list.
   * @param routes routes to search in.
   * @return sequence of airports used for travelling through shortest path if that path is found.
   *         Otherwise, a failure describing the problem.
   */
  def findShortestPath(departure: Airport,
                       arrival: Airport,
                       routes: Seq[Routes.Route]): Try[Seq[Routes.Route]] = {

    if (!Routes.groupAirports(routes).contains(departure)) {
      return Failure(InvalidAirport)
    }

    if (!Routes.groupAirports(routes).contains(arrival)) {
      return Failure(InvalidAirport)
    }

    if (arrival == departure) {
      return Failure(DepartureEqualToArrival)
    }

    val graph = Routes.buildGraph(routes)

    createTopologicalOrder(routes)
      .map(topOrder => createSSSP(graph, topOrder, departure).take(topOrder.indexOf(arrival) + 1).last)
      .map(_._2.get)
      .filter(_.nonEmpty)
      .orElse(Failure(NoRoutesFound))
  }
}
