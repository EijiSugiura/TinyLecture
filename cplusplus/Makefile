CPP=g++
CPPFLAGS=-Wall -g -std=gnu++11
LDFLAGS=-lboost_system -lboost_filesystem
OBJ=main.o filter.o
TARGET=daily-total

all: $(TARGET)

%.o: %.cc
	$(CPP) -c -o $@  $< $(CPPFLAGS)

daily-total: $(OBJ)
	$(CPP) -o $@ $^ $(CPPFLAGS) $(LDFLAGS)

clean:
	rm -f $(TARGET) $(OBJ) *~
