
CXXFLAGS = -O2 -Wall -g --std=c++11
LDFLAGS = -g -ljsoncpp
CXX = g++

OBJECTS = qsim.o

.SUFFIXES: .cxx .o .h

all: qsim

qsim: $(OBJECTS)
	$(CXX) $(LDFLAGS) -o qsim $(OBJECTS)

.cxx.o: $<
	$(CXX) -c $(CXXFLAGS) $<

$(OBJECTS): *.h

clean:
	rm -f $(OBJECTS) qsim
