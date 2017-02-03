CC=gcc
CFLAGS=-Wall -g
OBJ=main.o filter.o
TARGET=daily-total

all: $(TARGET)

%.o: %.c
	$(CC) -c -o $@  $< $(CFLAGS)

daily-total: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

clean:
	rm -f $(TARGET) $(OBJ) *~
