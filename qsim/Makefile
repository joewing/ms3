
CXXFLAGS = -O2 -march=native -Wall --std=c++11 -g
LDFLAGS = -static -g
CXX = g++

OBJECTS = qsim.o

.SUFFIXES: .cxx .o .hh

all: qsim

qsim: $(OBJECTS)
	$(CXX) $(LDFLAGS) -o qsim $(OBJECTS)

.cxx.o: $<
	$(CXX) -c $(CXXFLAGS) $<

$(OBJECTS): *.hh *.h

clean:
	rm -f $(OBJECTS) qsim
