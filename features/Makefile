
LIB=../etc
DIST=../dist
SCALAC=../scala2.7/bin/scalac

CP=${LIB}/lib.jar:${LIB}/aux.jar:${LIB}/scala-library.jar:${DIST}/generation.jar

NAME = features
SRC = src
CLASSES = bin


${NAME}.jar: $(SRC)/features/Sumtime.scala
	mkdir -p $(CLASSES)	
	$(SCALAC) -Xcheckinit -deprecation -d $(CLASSES) -cp $(CP) `find $(SRC) -name "*.scala"` `find $(SRC) -name "*.java"`
	#javac -d $(CLASSES) -classpath $(CP) `find $(SRC) -name "*.java"`
	jar cf $(NAME).jar -C $(CLASSES) .
	jar uf $(NAME).jar -C $(SRC) .

clean:
	rm -rf $(CLASSES) $(NAME).jar
