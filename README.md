# Multiparty Session Types

This project implements some key algorithms for multiparty session types. 

## Syntax 

### Global Types 
```
G := p -> q {l1: ..., ..., ln: ...} | t | rec t . G | end
```

### Local Types 
```
T := p ! {l1: ..., ..., ln: ...} | p ? {l1: ..., ..., ln: ...} | t | rec t . G | end
``` 
