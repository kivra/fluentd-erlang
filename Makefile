PROJECT = fluentd

# Dependecies ##########################################################
DEPS = stdlib2 msgpack jsx

dep_stdlib2 = git git@github.com:kivra/stdlib2.git          master
dep_jsx     = git git@github.com:talentdeficit/jsx.git      v2.1.1

# Standard targets #####################################################
include erlang.mk

ifneq ($(wildcard test/),)
ebin/$(PROJECT).app:: $(shell find test -type f -name \*.erl)
    $(if $(strip $?),$(call compile_erl,$?))
endif

eunit: ERLC_OPTS = $(TEST_ERLC_OPTS) # to have -ifdef(TEST) working properly
eunit: clean app
	$(gen_verbose) erl -noshell -pa ebin -eval 'eunit:test({application, $(PROJECT)}, [])' -s init stop

# eof
