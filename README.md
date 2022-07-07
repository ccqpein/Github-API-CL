# README #

This repo is the lite Github rest v3 api client. 

Because I cannot find the whole Github rest api in one file, just several documents pages. So you might need to add api details by yourself into json file. api.json is the example.

Documents link is [here](https://developer.github.com/v3/)

## Usage ##

You can check `./example`

### Dependencies ###

I use [yason](https://github.com/phmarek/yason) for json parser, [dexador](https://github.com/fukamachi/dexador) for http client.

### Install ###

This repo haven't deploy to quicklisp yet, but you can git clone to `quicklisp/local-projects` folder and `(ql:quickload "github-api-cl")`

### Read api.json file ###

Suppose we have `api.json` looks like:

``` json
{
    "repositories": {
        "repositories": {
          "List repositories for a user": {
                "parameters": [
                  ["type", "string"],
                  ["sort","string"],
                  ["direction","string"]
                ],
                "api": "GET /users/:username/repos",
                "link": "https://developer.github.com/v3/repos/#list-repositories-for-a-user"
            }
        }
    }
}
```
**Examples:**

```lisp
;; load system first
(ql:quickload "github-api-cl")

;; read api.json in this repo, path var is github-api-doc::*api-json-file-path*
(github-api-doc:read-api-json) ;; => return a hashtable of this json

;; OR you can give path specially
(github-api-doc:read-api-json #P"/path/to/api.json")
```

### Generate api instance ###

After read api.json file, you can generate api instance by using `github-api-doc:make-api-doc-from-json`

**Examples:**

``` lisp
;; read api.json
(defparameter *api-docs* (github-api-doc:read-api-json))

;; &rest arguments are the steps of reading json
(defparameter *api-doc* (github-api-doc:make-api-doc-from-json *api-docs* "repositories" "repositories" "List repositories for a user"))

;; Get api-doc: 
;;api-doc object:
;;  api: GET /users/:username/repos,
;;  http method: GET,
;;  slots: (:username),
;;  fmt-control: (/users/~a/repos)
;;  parameters: ((type string) (sort string) (direction string))

;; OR, you can make instance manually
(setf *api-doc* (make-instance 'api-doc
                               :api "GET /users/:username/repos"
                               :parameters '(("type" "string") 
                                             ("sort" "string") 
                                             ("direction" "string")))
```

`api.json` is pretty flexible because it just a json file. So you don't have to follow Github api structure if you don't want to. Only part of api.json does `github-api-cl` care about is this part:

```json
{
  "parameters": [
    ["type", "string"],
    ["sort","string"],
    ["direction","string"]
  ],
  "api": "GET /users/:username/repos",
}
```

You can read this json file and `(github-api-doc:make-api-doc-from-json (github-api-doc:read-api-json #P"this-simple-api.json"))`

### Make github-api client ###

Making github-api client:

```lisp
;; make instance of api-client
(defparameter *client-without-token* (make-instance 'github-client:api-client))

;; if you have token for github rest api call, make like this
(defparameter *client-with-token* (make-instance 'github-client:api-client :token "123"))
```

### Call api ###

With client and api, now we can call api in our code:

```lisp
;; call api with client and api we made before
(github-client:github-api-call *client-without-token*
                               *api-doc*)

;;; REPL will ask you to input `:username`, `type`, `sort`, and `direction`
;;; Then, it will return the dex:http-response, you can find this MULTIPLE-VALUEs 
;;; return format in https://github.com/fukamachi/dexador#following-redirects-get-or-head

;; call POST method api with additional :content keyword
(github-client:github-api-call *client-without-token*
                               *api-doc*
                               :headers '((header0 . value0) (header1 . value1))
                               :content "this argument pass to dexador directly")
```

`github-api-call` will call api with the default headers `'(("Accept" . "application/vnd.github+json"))`. Any other headers pass to `:headers` will been added `("Accept" . "application/vnd.github+json")`.

From now, github-api-cl's job is done, left all http response back to you, you can do whatever you want.

Wait, if you do not want REPL ask you to input every slots and parameters:

```lisp
(github-client:github-api-call *client-without-token*
                               *api-doc*
                               :username "lisp"
                               :type "public"
                               :direction "test"
                               :neither-slots-nor-parameter 1) ;; last keyword is redundant
```

With keywords input, REPL won't ask you anything, just call `https://api.github.com/users/lisp/repos?type=\"public\"&direction=\"test\"`. 

As example shows, `:username` fills api slot, `:type` & `:direction` fill parameters, `:neither-slots-nor-parameter` is useless in this api. 

For `POST` method api, `:content` is the keyword for add the content. It pass to `:content` keyword of `dexador`'s `POST` [method](https://github.com/fukamachi/dexador#function-post).

### Authorization ###

**Token, user-name&passd**

When you need authorization, you can put `:token`, `:user-name` and `:passd` in `github-client:github-api-call` as keywords. 

Check logic is below :

+ If you input `:token`, will use token you input
+ If no `:token` given but client has token already, it will use token stored in client
+ If neither `:token` nor client's token is given, but has `:passd` keyword, will use `:user-name` & `passd` as authorization. (I just assume you give `:user-name` too, when you give `:passd`)
