PROJ=slack-client

test:
	@ros -e '(push #p"$(shell pwd)/" asdf:*central-registry*)(ql:quickload "${PROJ}-test" :silent t)(prove:run #p"t/${PROJ}.lisp")'
