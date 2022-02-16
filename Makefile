run-example:
	mkdir -p example-dist
	spago run --main Example1
	dot -Tsvg example-dist/example-graph.dot > example-dist/example-graph.svg