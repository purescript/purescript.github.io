module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    clean: ["_site/css/*"],

    compass: {
      dev: {
        options: {
          sassDir: "sass",
          cssDir: "_site/css",
          fontsDir: "fonts",
          imagesDir: "img",
          relativeAssets: true,
          javascriptsDir: "js",
          outputStyle: "compressed"
        }
      }
    },

    connect: {
      dev: {
        options: {
          base: "."
        }
      }
    },

    watch: {
      sass: {
        files: "sass/**/*.scss",
        tasks: "compass"
      }
    }

  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-contrib-compass");
  grunt.loadNpmTasks("grunt-contrib-watch");
  grunt.loadNpmTasks("grunt-contrib-connect");

  grunt.registerTask("dev", ["connect", "watch"]);
  grunt.registerTask("default", ["clean", "compass"]);
};
