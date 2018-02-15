
## Looking at the acme files
```sh
9p ls acme/
```

```txt
19
20
22
acme
cons
consctl
draw
editout
index
label
log
new
```

## listing cons lists only cons
```sh
9p ls acme/cons
```

```txt
cons
```

## listing acme/acme lists nothing
```sh
9p ls acme/acme
```

```txt
```

## listing acme/cons/cons gives an error
```sh
9p ls acme/cons/cons
```

```txt
dirstat acme: not a directory
9p: exit 1
```

## listing non-existing directory
```sh
9p ls acme/foo
```

```txt

dirstat acme: file does not exist
9p: exit 1
```

## listing consctl lists only consctl
```sh
9p ls acme/consctl
```

```txt

consctl
```

## listing directories recursively
```sh
./9tree.hs
```

```txt
acme
    19
        addr
            addr
        body
            body
        ctl
            ctl
        data
            data
        editout
            editout
        errors
            errors
        event
            event
        rdsel
            rdsel
        tag
            tag
        wrsel
            wrsel
        xdata
            xdata
    20
        addr
            addr
        body
            body
        ctl
            ctl
        data
            data
        editout
            editout
        errors
            errors
        event
            event
        rdsel
            rdsel
        tag
            tag
        wrsel
            wrsel
        xdata
            xdata
    396
        addr
            addr
        body
            body
        ctl
            ctl
        data
            data
        editout
            editout
        errors
            errors
        event
            event
        rdsel
            rdsel
        tag
            tag
        wrsel
            wrsel
        xdata
            xdata
    423
        addr
            addr
        body
            body
        ctl
            ctl
        data
            data
        editout
            editout
        errors
            errors
        event
            event
        rdsel
            rdsel
        tag
            tag
        wrsel
            wrsel
        xdata
            xdata
    acme
    cons
        cons
    consctl
        consctl
    draw
    editout
        editout
    index
        index
    label
        label
    log
        log
    new
        addr
            addr
        body
            body
        ctl
            ctl
        data
            data
        editout
            editout
        errors
            errors
        event
            event
        rdsel
            rdsel
        tag
            tag
        wrsel
            wrsel
        xdata
            xdata
```

## listing files in the new directory creates a window
```
9p ls acme/new/addr
```

```txt
addr
```

## reading acme/index
```
9p read acme/index
```

```txt
        448          60        2771           0           0 /Users/jlariosmurillo/Scripts/acme.md Del Snarf Undo | Look 
         19          44         809           1           0 /Users/jlariosmurillo/ Del Snarf Get | Look 
         20          52         217           1           0 /Users/jlariosmurillo/Scripts/ Del Snarf Get | Look 
        452          55         339           0           1 /Users/jlariosmurillo/Scripts/+Errors Del Snarf | Look 

```

## reading acme/$winid/tag
```
9p read acme/$winid/tag
```

```txt
/Users/jlariosmurillo/Scripts/acme.md Del Snarf Undo Put | Look 
```

## reading acme/$winid/addr

```
9p read acme/$winid/addr
```

```txt
          0           0 
```

## reading acme/$winid/addr after writing dot in ctl

```
echo 'addr=dot' | 9p write acme/$winid/ctl
9p read acme/$winid/data
```
