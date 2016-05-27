#include "arith.h"

#include <limits.h>
#include <stdint.h>

#include "util.h"

size_t size_mul(size_t x, size_t y) {
  CHECK(y == 0 || x <= SIZE_MAX / y);
  return x * y;
}

size_t size_add(size_t x, size_t y) {
  CHECK(x <= SIZE_MAX - y);
  return x + y;
}

size_t size_sub(size_t x, size_t y) {
  CHECK(x >= y);
  return x - y;
}

int32_t size_to_int32(size_t x) {
  STATIC_CHECK(INT32_MAX <= SIZE_MAX);
  CHECK(x <= (size_t)INT32_MAX);
  return (int32_t)x;
}

int size_to_int(size_t x) {
  STATIC_CHECK(INT_MAX <= SIZE_MAX);
  CHECK(x <= (size_t)INT_MAX);
  return (int)x;
}

int try_uint32_mul(uint32_t x, uint32_t y, uint32_t *out) {
  if (y == 0 || x <= UINT32_MAX / y) {
    *out = x * y;
    return 1;
  } else {
    return 0;
  }
}

uint32_t uint32_mul(uint32_t x, uint32_t y) {
  uint32_t ret;
  int success = try_uint32_mul(x, y, &ret);
  CHECK(success);
  return ret;
}

int try_uint32_add(uint32_t x, uint32_t y, uint32_t *out) {
  if (x <= UINT32_MAX - y) {
    *out = x + y;
    return 1;
  } else {
    return 0;
  }
}

uint32_t uint32_add(uint32_t x, uint32_t y) {
  uint32_t ret;
  int success = try_uint32_add(x, y, &ret);
  CHECK(success);
  return ret;
}

int try_uint32_sub(uint32_t x, uint32_t y, uint32_t *out) {
  if (x < y) {
    return 0;
  }
  *out = x - y;
  return 1;
}

uint32_t uint32_sub(uint32_t x, uint32_t y) {
  uint32_t ret;
  int success = try_uint32_sub(x, y, &ret);
  CHECK(success);
  return ret;
}

int try_uint32_div(uint32_t x, uint32_t y, uint32_t *out) {
  if (y == 0) {
    return 0;
  }
  *out = x / y;
  return 1;
}
int try_uint32_mod(uint32_t x, uint32_t y, uint32_t *out) {
  if (y == 0) {
    return 0;
  }
  *out = x % y;
  return 1;
}

uint32_t uint32_ceil_aligned(uint32_t x, uint32_t m) {
  CHECK(m != 0);
  uint32_t y = x % m;
  if (y == 0) {
    return x;
  } else {
    return uint32_add(x, m - y);
  }
}

uint32_t uint32_max(uint32_t x, uint32_t y) {
  return x < y ? y : x;
}



uint32_t size_to_uint32(size_t x) {
  CHECK(x <= UINT32_MAX);
  return x;
}

size_t uint32_to_size(uint32_t x) {
  CHECK(x <= SIZE_MAX);
  return x;
}


int try_uint8_add(uint8_t x, uint8_t y, uint8_t *out) {
  if (x > UINT8_MAX - y) {
    return 0;
  } else {
    *out = x + y;
    return 1;
  }
}
int try_uint8_sub(uint8_t x, uint8_t y, uint8_t *out) {
  if (x < y) {
    return 0;
  } else {
    *out = x - y;
    return 1;
  }
}
int try_uint8_mul(uint8_t x, uint8_t y, uint8_t *out) {
  uint32_t prod = ((uint32_t)x) * ((uint32_t)y);
  if (prod <= UINT8_MAX) {
    *out = (uint8_t)prod;
    return 1;
  } else {
    return 0;
  }
}

int try_uint8_div(uint8_t x, uint8_t y, uint8_t *out) {
  if (y == 0) {
    return 0;
  } else {
    *out = x / y;
    return 1;
  }
}

int try_uint8_mod(uint8_t x, uint8_t y, uint8_t *out) {
  if (y == 0) {
    return 0;
  } else {
    *out = x % y;
    return 1;
  }
}


int try_int32_add(int32_t x, int32_t y, int32_t *out) {
  if (x < 0 && y < 0) {
    if (x < INT32_MIN - y) {
      return 0;
    }
  } else if (x >= 0 && y >= 0) {
    if (x > INT32_MAX - y) {
      return 0;
    }
  }

  *out = x + y;
  return 1;
}

int32_t int32_add(int32_t x, int32_t y) {
  int32_t ret;
  int success = try_int32_add(x, y, &ret);
  CHECK(success);
  return ret;
}

int try_int32_sub(int32_t x, int32_t y, int32_t *out) {
  if (x < 0 && y > 0) {
    if (x < INT32_MIN + y) {
      return 0;
    }
  } else if (x >= 0 && y <= 0) {
    if (x > INT32_MAX + y) {
      return 0;
    }
  }
  *out = x - y;
  return 1;
}

int32_t int32_sub(int32_t x, int32_t y) {
  int32_t ret;
  int success = try_int32_sub(x, y, &ret);
  CHECK(success);
  return ret;
}

int try_int32_mul(int32_t x, int32_t y, int32_t *out) {
  int64_t xll = x;
  int64_t yll = y;
  int64_t p = xll * yll;
  if (p < INT32_MIN || p > INT32_MAX) {
    return 0;
  }
  *out = (int32_t)p;
  return 1;
}

int32_t int32_div(int32_t x, int32_t y) {
  CHECK(y != 0 && !(x == INT32_MIN && y == -1));
  return x / y;
}

int32_t int32_positive_mod(int32_t x, int32_t y) {
  CHECK(x >= 0 && y > 0);
  return x % y;
}

int try_int32_to_uint32(int32_t x, uint32_t *out) {
  if (x >= 0) {
    *out = (uint32_t)x;
    return 1;
  } else {
    return 0;
  }
}

int try_int32_to_uint8(int32_t x, uint8_t *out) {
  if (x >= 0 && x <= UINT8_MAX) {
    *out = (uint8_t)x;
    return 1;
  } else {
    return 0;
  }
}

int try_uint32_to_int32(uint32_t x, int32_t *out) {
  if (x <= (uint32_t)INT32_MAX) {
    *out = (int32_t)x;
    return 1;
  } else {
    return 0;
  }
}

int try_uint32_to_uint8(uint32_t x, uint8_t *out) {
  if (x <= UINT8_MAX) {
    *out = (uint8_t)x;
    return 1;
  } else {
    return 0;
  }
}

int32_t uint32_to_int32(uint32_t x) {
  int32_t ret;
  int success = try_uint32_to_int32(x, &ret);
  CHECK(success);
  return ret;
}

size_t uint64_to_size(uint64_t x) {
  CHECK(x <= SIZE_MAX);
  return x;
}
