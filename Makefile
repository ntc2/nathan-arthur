all : trains

PAKCS=pakcs

%.state : %.curry
	$(PAKCS) -s $<
% : %.state
	cp -a $< $@

run : trains
	./trains

clean :
	-rm -rf *.state
	-cleancurry