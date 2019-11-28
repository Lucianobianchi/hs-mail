# haskell-mail
Haskell implementations of basic SMTP and POP3 servers 

## Installation
Make sure you have GHC and [Stack](https://docs.haskellstack.org/en/stable/README/) installed, then run:

```
stack install
```

These implementations use a local [Redis](https://redis.io/) databas. Make sure you have one running at `127.0.0.1:6379` before running the program.

## Run

```
stack run
```

Then you can connect to each server in its corresponding port: 


```
$ nc localhost 25 /* SMTP server */
$ nc localhost 110 /* POP3 server */
```