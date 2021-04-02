object Routes {
  case class Airport(iataCode: String)
  case class DepartureAirport(override val iataCode: String) extends Airport(iataCode)
  case class ArrivalAirport(override val iataCode: String) extends Airport(iataCode)
  case class Route(departureAirport: DepartureAirport, arrivalAirport: ArrivalAirport, durationHours: Int)

  val providedRoutes = Set(
    Route(DepartureAirport("DUB"), ArrivalAirport("LHR"), 1),
    Route(DepartureAirport("DUB"), ArrivalAirport("CDG"), 2),
    Route(DepartureAirport("CDG"), ArrivalAirport("BOS"), 6),
    Route(DepartureAirport("CDG"), ArrivalAirport("BKK"), 9),
    Route(DepartureAirport("ORD"), ArrivalAirport("LAS"), 2),
    Route(DepartureAirport("LHR"), ArrivalAirport("NYC"), 5),
    Route(DepartureAirport("NYC"), ArrivalAirport("LAS"), 3),
    Route(DepartureAirport("BOS"), ArrivalAirport("LAX"), 4),
    Route(DepartureAirport("LHR"), ArrivalAirport("BKK"), 9),
    Route(DepartureAirport("BKK"), ArrivalAirport("SYD"), 11),
    Route(DepartureAirport("LAX"), ArrivalAirport("LAS"), 2),
    Route(DepartureAirport("DUB"), ArrivalAirport("ORD"), 6),
    Route(DepartureAirport("LAX"), ArrivalAirport("SYD"), 13),
    Route(DepartureAirport("LAS"), ArrivalAirport("SYD"), 14),
  )
}
