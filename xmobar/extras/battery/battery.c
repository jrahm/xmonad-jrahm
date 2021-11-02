#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <stdint.h>
#include <math.h>

#define N_ICONS 5

char* icons[] = {
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
};

typedef long long llong_t;

llong_t get_number(const char* file)
{
  int fd;
  char buf[128];
  fd = open(file, O_RDONLY);

  if (fd < 0) {
    return -1;
  }

  size_t count = read(fd, buf, sizeof(buf));
  close(fd);

  if (count < 0 || count > sizeof(buf) - 1) {
    return -1;
  }

  buf[count] = 0;
  return atoll(buf);
}

float absf(float f)
{
  return f < 0 ? -f : f;
}

uint32_t hsv_to_rgb(int h, int s, int v){
  if(h > 360 || h < 0 || s > 100 || s < 0 || v > 100 || v < 0){
    return -1;
  }

  float sf = s / 100.0;
  float vf = v / 100.0;
  float hf = (float) h;

  float c = sf * vf;
  float x = c * (1 - absf(fmod(hf / 60.0, 2) - 1));
  float m = vf - c;
  float r,g,b;

  if(h >= 0 && h < 60){
    r = c;
    g = x;
    b = 0;
  }
  else if(h >= 60 && h < 120){
    r = x;
    g = c;
    b = 0;
  }
  else if(h >= 120 && h < 180){
    r = 0;
    g = c;
    b = x;
  }
  else if(h >= 180 && h < 240){
    r = 0;
    g = x;
    b = c;
  }
  else if(h >= 240 && h < 300){
    r = x;
    g = 0;
    b = c;
  }
  else{
    r = c;
    g = 0;
    b = x;
  }

  int ri = (int) ((r + m) * 255);
  int gi = (int) ((g + m) * 255);
  int bi = (int) ((b + m) * 255);

  return (ri << 16) | (gi << 8) | bi;
}

llong_t get_capacity()
{
  return get_number("/sys/class/power_supply/BAT0/capacity");
}

llong_t get_energy_full()
{
  return get_number("/sys/class/power_supply/BAT0/energy_full");
}

llong_t get_energy_now()
{
  return get_number("/sys/class/power_supply/BAT0/energy_now");
}

llong_t get_ac_online()
{
  return get_number("/sys/class/power_supply/AC/online");
}

llong_t get_power()
{
  return get_number("/sys/class/power_supply/BAT0/power_now");
}

int get_status(char* buf, size_t size)
{
  int fd;
  fd = open("/sys/class/power_supply/BAT0/status", O_RDONLY);

  if (fd < 0) {
    return 1;
  }

  size_t count = read(fd, buf, size);
  close(fd);

  if (count < 0 || count > size - 1) {
    return 1;
  }

  buf[count] = 0;
  --count;
  while (count > 0 && buf[count] == '\n') {
    buf[count --] = 0;
  }

  return 0;
}

uint32_t percentage_to_color(int percentage)
{
  int h = 135 * percentage / 100; 
  int s = 81;
  int v = 76;

  return hsv_to_rgb(h, s, v);
}

void get_time_left(char* buf, size_t sz, llong_t energy, llong_t power)
{
  llong_t minutes_left = energy * 60 / power;

  llong_t hours = minutes_left / 60;
  llong_t minutes = minutes_left % 60;

  snprintf(buf, sz - 1, "%2lluh%2llum", hours, minutes);
  buf[sz - 1] = 0;
}

int main(int argc, char** argv)
{
  char* icon;
  char timeleft[128];

  llong_t capacity;
  llong_t energy_now;
  llong_t ac_online;
  llong_t power;

  if ((energy_now = get_energy_now()) < 0) {
    fprintf(stderr, "Unable to get current energy.");
    goto fail;
  }

  if ((capacity = get_capacity()) < 0) {
    fprintf(stderr, "Unable to get capacity.");
    goto fail;
  }

  if ((ac_online = get_ac_online()) < 0) {
    fprintf(stderr, "Unable to get status.");
    goto fail;
  }

  if ((power = get_power()) < 0) {
    fprintf(stderr, "Unable toget power.");
    goto fail;
  }
  
  ac_online = !! ac_online;

  int percentage = (int) capacity;
  if (percentage >= 100) {
    icon = icons[10 + ac_online];
  } else {
    int quintile = percentage / 20;
    icon = icons[quintile + (5 * ac_online) ];
  }

  get_time_left(timeleft, sizeof(timeleft), energy_now, power);

  double dpower = power / 1000000.0;
  uint32_t color = percentage_to_color(percentage);
  printf("<fc=#%06x>%s </fc><fc=#8888ff>%d%% %2.1fW %s</fc>", color, icon, percentage, dpower, timeleft);
  return 0;

fail:
  printf("");
  return 0;
}
