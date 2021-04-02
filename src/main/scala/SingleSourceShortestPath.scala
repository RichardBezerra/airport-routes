import Routes.{Airport, groupAirports}

import scala.collection.mutable
import scala.util.{Success, Try}

object SingleSourceShortestPath {

  def createTopologicalOrder(graph: Map[Airport, Seq[Routes.Route]]): Try[Seq[Airport]] = {
    // Map[Airport, amount of routes that have this airport as arrival].
    // It could be the graph instead of the routes.
    val initialAmountArrivalConnections: Map[Airport, Int] = groupAirports(Routes.providedRoutes)
      .map(airport => (airport, Routes.providedRoutes.count(_.arrival == airport))).toMap

    // A mutable Map[Airport, amount of routes that have this airport as arrival]
    // used to track the the decrement of connections while topological order is being created.
    val mutableAmountArrivalConnections: mutable.Map[Airport, Int] = mutable.Map.from(initialAmountArrivalConnections)

    // Initiate with airports that are not arrival of any route
    val airportsToProcess: mutable.Queue[Airport] =
      mutable.Queue.from(initialAmountArrivalConnections.filter(_._2 == 0).keys)

    var topologicalOrder: Seq[Airport] = Seq()

    while (airportsToProcess.nonEmpty) {
      val airport = airportsToProcess.dequeue()
      topologicalOrder = topologicalOrder :+ airport

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

    Success(topologicalOrder)
  }
}
