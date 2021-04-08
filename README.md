# Airport Routes challenge

## Problem analysis
The routes provided along with challenge description might impact the understanding of the problem. The provided routes cover only flights to same direction as per picture below.  So, considering only the routes, the problem is simpler than the generic description. In the other side, considering that could exist routes connecting airports in both directions, the problem gets more complex.

![Provided routes](doc/provided-routes.png)

## Two solutions
Because of these different point of views about the problem, the program cover each perspective with a different solution. Both solutions are built on top of graph algorithms.

### Assuming only the provided routes
In this case, the problem is faced as a Directed Acyclic Graph (DAG). The program creates a DAG based on the provided routes. Then, it generates a Topological Order on top of that and traverse the topological order searching for the shortest path between the departure and arrival informed airports.

### Extending with returning routes
In this case, the program duplicates each route creating a new route connecting same airports in the opposite direction as per picture below. So, the problem is faced as a Directed Cycle Graph. The program generates the graph from the extended routes an then applies a Dijkstra based algorithm able to find shortest paths in that kind of graph.

![Provided routes](doc/extended-routes.png)

In order to run the program in this mode, a flag `with-returning-routes` should be informed as the third parameter after the airports. Then, paths like BKK -> ORD are made possible.

## How to execute

Pre-Requisite: JRE 11 (it is likely to work on JRE 8 as well)

Download artifact from the [pipeline](https://github.com/RichardBezerra/airport-routes/actions/runs/727056867) at https://github.com/RichardBezerra/airport-routes/actions/runs/727056867.

- Default mode (with provided routes): `java -jar ./airport-routes-finder-0.1.jar DUB SYD`
- Extended mode (with returning routes): `java -jar ./airport-routes-finder-0.1.jar DUB SYD with-returning-routes`
