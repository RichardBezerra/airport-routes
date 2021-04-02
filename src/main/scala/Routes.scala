import scala.collection.IterableOnce.iterableOnceExtensionMethods

object Routes {

  case class Airport(iataCode: String)

  case class Route(departure: Airport, arrival: Airport, durationHours: Int)

  val providedRoutes = Seq(
    Route(Airport("DUB"), Airport("LHR"), 1),
    Route(Airport("DUB"), Airport("CDG"), 2),
    Route(Airport("CDG"), Airport("BOS"), 6),
    Route(Airport("CDG"), Airport("BKK"), 9),
    Route(Airport("ORD"), Airport("LAS"), 2),
    Route(Airport("LHR"), Airport("NYC"), 5),
    Route(Airport("NYC"), Airport("LAS"), 3),
    Route(Airport("BOS"), Airport("LAX"), 4),
    Route(Airport("LHR"), Airport("BKK"), 9),
    Route(Airport("BKK"), Airport("SYD"), 11),
    Route(Airport("LAX"), Airport("LAS"), 2),
    Route(Airport("DUB"), Airport("ORD"), 6),
    Route(Airport("LAX"), Airport("SYD"), 13),
    Route(Airport("LAS"), Airport("SYD"), 14)
  )

  def retrieveAirports(routes: Seq[Routes.Route]): Set[Airport] = {
    (routes.map(_.departure) :++ routes.map(_.arrival)).toSet
  }

  def buildGraph(routes: Seq[Routes.Route]): Map[Airport, Seq[Routes.Route]] = {
    routes.map(_.departure).distinct.map(dep => dep -> routes.filter(_.departure == dep)).toMap
  }

  def buildDegreesTracking(graph: Map[Airport, Seq[Routes.Route]]): Seq[(Airport, Int)] = {
    // traverse 'graph' rather than 'providedRoutes'
    retrieveAirports(providedRoutes)
      .map(airport => (airport, providedRoutes.count(_.arrival == airport))).toSeq
  }
}
