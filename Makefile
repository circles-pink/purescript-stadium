run-example:
	spago run --main Example1 > example-dist/example-graph.dot
	dot -Tsvg example-dist/example-graph.dot > example-dist/example-graph.svg