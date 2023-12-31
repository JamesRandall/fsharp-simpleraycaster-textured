# F# / Simple Textured Raycaster

Basic raycaster implementation that attempts to look a bit like Wolfenstein.

Most of the rest of the docs come from the Fable starter project but apply to running this yourself.

## Asset Extraction

Didn't really want to process WLx files by myself so I used a couple of tools to extract the assets.

Firstly [Wolf3dExtract](https://github.com/HiPhish/Wolf3DExtract) to retrieve the components and then [ImageMagick](https://imagemagick.org/) to convert textures from PPM to PNG format. 

## Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 5.0 or higher
* [node.js](https://nodejs.org)
* An F# editor like Visual Studio, Visual Studio Code with [Ionide](http://ionide.io/) or [JetBrains Rider](https://www.jetbrains.com/rider/)

## Building and running the app

* Install dependencies: `npm install`
* Start the compiler in watch mode and a development server: `npm start`
* After the first compilation is finished, in your browser open: http://localhost:8080/

Any modification you do to the F# code will be reflected in the web page after saving.

> Note: check the "scripts" section in `package.json` to see the commands triggered by the steps above.

## Bundling for release

Run the following command to compile and bundle up all your F# code into one Javascript file: `npm run build`. The compiled output ends up in the `public` folder under the name `bundle.js`.

## Project structure

### npm

JS dependencies are declared in `package.json`, while `package-lock.json` is a lock file automatically generated.

### Webpack

[Webpack](https://webpack.js.org) is a JS bundler with extensions, like a static dev server that enables hot reloading on code changes. Configuration for Webpack is defined in the `webpack.config.js` file. Note this sample only includes basic Webpack configuration for development mode, if you want to see a more comprehensive configuration check the [Fable webpack-config-template](https://github.com/fable-compiler/webpack-config-template/blob/master/webpack.config.js).

### F#

The sample only contains two F# files: the project (.fsproj) and a source file (.fs) in the `src` folder.

### Web assets

The `index.html` file and other assets like an icon can be found in the `public` folder.
