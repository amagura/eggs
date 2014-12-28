#if 0 // LICENSE
Copyright 2014 Alexej Magura

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
#endif

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdint.h>

uintmax_t
epoch()
{
  time_t secs = time(NULL);

  if (secs == ((time_t)-1))
    return -2;
  return (uintmax_t)secs;
}

int main()
{
  printf("%ju\n", epoch());
  return 0;
}
