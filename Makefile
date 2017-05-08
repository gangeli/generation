# -- VARIABLES --
# (locations)
SRC=src
TEST_SRC=test/src
LIB=etc
BUILD=bin
TEST_BUILD=test/bin
DIST=dist
TMP=tmp
# (classpath)
CP=${LIB}/lib.jar:${LIB}/aux.jar:${LIB}/scala-library.jar


# -- JARS --
generation.jar: $(foreach dir, ${SRC}, $(wildcard ${dir}/*))
	mkdir -p ${BUILD}
	mkdir -p ${DIST}
	javac -d $(BUILD) -cp $(CP) `find $(SRC) -name "*.java"`
#	fsc -deprecation -d ${BUILD} -cp ${CP} `find ${SRC} -name "*.scala"` `find ${SRC} -name "*.java"` # faster, but on rare occasion buggy
	scala2.7/bin/scalac -deprecation -d ${BUILD} -cp ${CP} `find ${SRC} -name "*.scala"` `find ${SRC} -name "*.java"`
	jar cf ${DIST}/generation.jar -C $(BUILD) .
	jar uf ${DIST}/generation.jar -C $(SRC) .

# -- TARGETS --
default: ${DIST}/generation.jar 

clean:
	rm -rf ${BUILD}
	rm -rf ${TEST_BUILD}
	rm -rf ${DIST}
	rm -f java.hprof.txt
