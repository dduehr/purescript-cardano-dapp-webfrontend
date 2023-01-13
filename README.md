# Cardano DApp Web-Frontend

[![Purescript](https://img.shields.io/badge/Purescript-v0.15.5-1d222d)](https://www.purescript.org/)
[![Purescript Halogen UI Framework](https://img.shields.io/badge/Purescript%20Halogen-v7.0.0-blue)](https://github.com/purescript-halogen/purescript-halogen)
[![Purescript CIP-30](https://img.shields.io/badge/Purescript%20CIP--30-v0.0.1-b5458f)](https://github.com/anton-k/purescript-cip30)
[![Purescript CSL](https://img.shields.io/badge/Purescript%20CSL-v0.0.1-b5458f)](https://github.com/anton-k/purescript-cardano-serialization-lib)
[![Rust CSL](https://img.shields.io/badge/Rust%20CSL-v11.2.1-f66a00)](https://github.com/Emurgo/cardano-serialization-lib)
[![Bulma CSS](https://img.shields.io/badge/Bulma%20CSS-v0.9.4-02d1b2)](https://bulma.io/)

This is an exemplary Cardano dApp web-frontend application implemented with PureScript and the Halogen framework.

It offers the ability to interact with the Cardano blockchain by sending ADA and native token to public-key addresses or Plutus contracts or redeem these also. The development ist still in progress. Transactions besides sending ADA to Cardano public-key addresses are not implemented yet.

The application showcases a Cardano dApp web-frontend written with a strongly-typed functional programming language. In my opinion a perfect fit to the Cardano development ecosystem. 

It utilizes the PureScript interface to Cardano wallets as specified by [CIP 30 - Cardano dApp-Wallet Web Bridge](https://cips.cardano.org/cips/cip3).

A live demo can be found at https://bit.ly/purescript-cardano-dapp-webfrontend.

## Installation

Unless already done install the Node package manager [`npm`](https://docs.npmjs.com/), see [Downloading and installing Node.js and npm](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm).

Additionally required are the PureScript compiler [`purs`](https://www.npmjs.com/package/purescript), the PureScript syntax tidy-upper [`purs-tidy`](https://www.npmjs.com/package/purs-tidy), the PureScript package manager [`spago`](https://www.npmjs.com/package/spago) and the JavaScript bundler and minifier [`esbuild`](https://www.npmjs.com/package/esbuild). 

Install the tools using the Node package manager:

```console
npm install -g purescript
npm install -g purs-tidy
npm install -g spago
npm install -g esbuild
```

Finally install the the web-frontend application package itself and it's dependencies:

```console
npm install
```

## Building and Running

Just run the `serve` script to incrementally format, build, bundle, package and serve the web-application from http://localhost:8080/ in one single step:

```console
npm run serve
```

### Scripts

The particular steps to `format`, `build`, `bundle`, `dist` and finally `serve` the application files are defined in the scripts section of the file `package.json`. A scripts is executed by `npm run` *\<script\>*.

Overview of the `npm run` scripts:

| Script | Effect |
| --- | --- |
| format | Format the PureScript files in-place |
| build | Compile the PureScript files |
| bundle | Bundle the PureScript files into one file in the folder `bundle` |
| dist | Package the application files into the folder `dist` for distribution |
| serve | Serve the application from the distribution folder `dist` |
| clean | Remove the generated script artifacts again |

The scripts are executed incrementally in the order as specified above. For instance, if you run `dist` to create the distribution files, the scripts `format`, `build` and `bundle` are executed beforehand automatically .

## Design

The application is structured according to the blog post [Three Layer Haskell Cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html) by Matt Parsons and particularly inspired by the [Real World Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld) application by Thomas Honeyman.

The module `AppM` serves as orchestrating "Layer 1". it wires up the capabilities to it's actual implementations.

The capabilities serve as interfaces and are specified by the "Layer 2" type classes. The are divided into "application domain" capabilities and "application infrastructure" capabilities. The latter mock out external dependencies (Cardano CIP-30 wallet, Cardano serialization library) and are applied by the first. The "application domain" capabilities are the foundation for the implementation of the application business logic. 

The implementation of the type classes are provided by the application API as "Layer 3". It is divided into a "application domain" API and "application infrastructure" API correspondingly.   

## Distribution

In order to experimentally serve the the "`dist`" files of the application by an http server like [Apache httpd](https://httpd.apache.org/) or [nginx](https://www.nginx.com/) ensure the [WebAssembly MIME type](https://www.iana.org/assignments/media-types/application/wasm) is configured correctly.

Exemplary [.htaccess](https://httpd.apache.org/docs/2.4/howto/htaccess.html) file for the Apache httpd server:

```console
AddType application/wasm .wasm
```
## Acknowledgements

I'd like to thank Anton Kholomiov for his PureScript FFI bindings to JavaScript Cardano CIP-30 wallet interface and [Cardano Serialization Library](https://github.com/Emurgo/cardano-serialization-lib) by emurgo written in Rust. Antons' libraries showed up in Oktober 2022, just at the time when I was thinking about a first PureScript project by myself.

* [Purescript interface to Cardano Wallets over Cip30](https://github.com/anton-k/purescript-cip30)
* [Purescript Cardano serialization library](https://github.com/anton-k/purescript-cardano-serialization-lib)

Thanks to Dimitry Shibaev et al. for their React JS boilerplate project. It gave me lots of insights about setting up Cardano transactions and eventually the inspiration for the use cases.

* [Cardano DApp Wallet Connector — A JavaScript and React JS app](https://github.com/dynamicstrategies/cardano-wallet-connector)

And in particular I'd like to thank Thomas Honeyman et al. for their great work about Halogen and the complementary libraries. First I was tempted to start this project with [Elm](https://guide.elm-lang.org/) but I quickly opted for PuresScript with Halogen. A powerful language with a  unopinionated and concise component based UI framework.  

* [Halogen — A type-safe library for building user interfaces in PureScript](https://github.com/purescript-halogen/purescript-halogen)
* [Real World Halogen — Large enough to demonstrate real world examples](https://github.com/thomashoneyman/purescript-halogen-realworld)
* [Formless — Helps you write forms in Halogen without the boilerplate](https://github.com/thomashoneyman/purescript-halogen-formless)
* [Halogen Store — Global state management for Halogen](https://github.com/thomashoneyman/purescript-halogen-store)

