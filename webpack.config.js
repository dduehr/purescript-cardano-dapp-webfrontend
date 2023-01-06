const HtmlWebpackPlugin = require('html-webpack-plugin');
const path = require('path');

const isProduction = process.env.NODE_ENV == 'production';

const config = {
    entry: './index.ts',
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'index.js'
    },
    devServer: {
        open: true,
        host: 'localhost'
    },    
    plugins: [
        new HtmlWebpackPlugin({
            template: 'index.html'
        })
    ],
    experiments: {
        asyncWebAssembly: true
    },
    ignoreWarnings: [
        { module: /@emurgo\/cardano-serialization-lib-browser\/cardano_serialization_lib_bg\.js/ }
    ]
};

module.exports = () => {
    if (isProduction) {
        config.mode = 'production';
    } else {
        config.mode = 'development';
    }
    return config;
};