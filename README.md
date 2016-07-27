# mop
mop0, basic0, judge0, and dmap0 are original version by "INSIDE CASE BASED REASONING".
run-judge-demo0 and run-dmap-demo0 are also the demo results on judge0 and dmap0.

## Loding with Quicklisp

```
$ cd ~/quicklisp/local-projects
$ git clone https://github.com/SeijiKoide/mop.git
```
and

```cl
(asdf:initialize-source-registry)
(ql:quickload :icbr-mop)
```

## Running the demos

```cl
(dmap-demo)
```

```cl
(judge-demo)
```

