#!/usr/bin/python3


# 需要将 MMA 的 RandomChoice 实现为一个纯函数 => random 模块 choice 函数
# 需要将 Transitter 实现为一个 Callable 对象

from typing import Dict
from random import choice, choices

class Transitter(object):
    def __init__(self, spec:Dict[]): ...
    def __call__(self, multi_index):
        if   self._key == 'byte':
            _index = multi_index.tobytes()
        elif self._key == 'tuple':
            _index = to_tuple(multi_index.tolist())
        else: raise NotImplementedError
        if _index not in self._loc:
            for nrow, row in enumerate(matIC):
                for ncol, some_index in enumerate(row[nrow:]):
                    if not (multi_index-some_index).any():
                        self._loc[_index] = (nrow, ncol)
                        break
        try: return self._loc[_index]
        except KeyError:
            print("existing locs:\n", self._loc)
            raise

###############################################################################

def main():
      ...

if __name__ == '__main__':
    main()