struct Pt {
  int x;
  int y;
};
struct Pt points[10];

double max(double a, double b) {
  if (a > b) {
    a = points[3].x;
    return a;
  } else {
    return b;
  }
}

int len(char s[]) {
  int i;
  i = 0;
  while (s[i])
	  i = i + 1;
  return i;
}

void ceva() {
  int i;
  i = 0;
  while (i < 10) {
    points[i].x = i;
    points[i].y = i * i;
    i = i + 1;
  }
}

void puti(int i) {
  printf("%d\n", i);
}

void main() {
  int i;
  i = 10;
  if (i != 5) {
	puti(i);
  } else {
	puti(5);
  }
  while (i != 0) {
    puti(i);
    i = i / 2;
	i = i * 5;
	i = i - 1;
  }
}
