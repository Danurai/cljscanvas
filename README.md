# ZZBase

## Overview

Base http-kit server with hiccup server-side pages and reagent cljs capability & figwheel reload.
Runs locally on PORT 9009

    http://localhost:9009

## Setup

update the following when renaming the project

### Folder Structure

    rename \src\zzbase to \src\<projectname>

###\project.clj
###\dev\user.clj
###\src\zzbase\core.cljs
###\src\zzbase\pages.cljs
###\src\zzbase\system.clj
###\src\zzbase\web.clj

    Find and replace zzbase with <project name>

## Running

### Interactive Mode

From project root launch a REPL and start using Component

    ####\zzbase>lein repl
    nREPL server started on port 57250 on host 127.0.0.1 - nrepl://127.0.0.1:57250
    REPL-y 0.3.7, nREPL 0.2.12
    Clojure 1.9.0-beta4
    Java HotSpot(TM) Client VM 1.8.0_191-b12
        Docs: (doc function-name-here)
              (find-doc "part-of-name-here")
      Source: (source function-name-here)
     Javadoc: (javadoc java-object-or-class-here)
        Exit: Control+D or (exit) or (quit)
     Results: Stored in vars *1, *2, *3, an exception in *e
    
    ####zzbase.system=> (ns user)
    nil
    ####user=> (reset)
    :reloading (zzbase.pages zzbase.web zzbase.system user)
    Server started on http://localhost:9009
    :resumed
    ####user=> (fig-start)
    Figwheel: Starting server at http://0.0.0.0:3449
    Figwheel: Watching build - dev
    Figwheel: Cleaning build - dev
    ←[0mCompiling "resources/public/js/compiled/zzbase.js" from ["src"]...←[0m
    ←[32mSuccessfully compiled "resources/public/js/compiled/zzbase.js" in 24.964 seconds.←[0m
    Figwheel: Starting CSS Watcher for paths  ["resources/public/css"]
    \zzbase>

### uberjar

From project root create and run standalone uberjar

    ####\zzbase>lein uberjar
    Compiling zzbase.pages
    Compiling zzbase.system
    Compiling zzbase.web
    Compiling zzbase.pages
    Compiling zzbase.system
    Compiling zzbase.web
    Compiling ClojureScript...
    Compiling ["resources/public/js/compiled/zzbase.js"] from ["src"]...
    ←[32mSuccessfully compiled ["resources/public/js/compiled/zzbase.js"] in 27.312 seconds.←[0m
    Created D:\projects\zzbase\target\zzbase.jar
    Created D:\projects\zzbase\target\zzbase-standalone.jar
    
    ####\zzbase>java -jar target\zzbase-standalone.jar
    Server started on http://localhost:9009

    
Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.