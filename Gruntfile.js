module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    clean: ["css/*"],

    compass: {
      dev: {
        options: {
          sassDir: "sass",
          cssDir: "css",
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
