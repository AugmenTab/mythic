module.exports = function(grunt) {
  const DEV_PATH = "/home/foundry/foundrydata/Data/systems/mythic/";

  // Configuration
  grunt.initConfig({
    // grunt-contrib-copy
    copy: {
      dev: {
        files: [
          {expand: true, src: "lang/*",        dest: DEV_PATH},
          {expand: true, src: "mythic.css",    dest: DEV_PATH},
          {expand: true, src: "system.json",   dest: DEV_PATH},
          {expand: true, src: "template.json", dest: DEV_PATH},
          {expand: true, src: "module/**",     dest: DEV_PATH},
          {expand: true, src: "templates/**",  dest: DEV_PATH},
          {expand: true, src: "mythic.js",     dest: DEV_PATH}
        ]
      }
    },

    // grunt-contrib-less
    less:{
      main: {
        options: {
          paths: ["less/"],
          compress: true
        },
        files: {
          "mythic.css": "less/mythic.less"
        }
      }
    },

    // grunt-contrib-watch
    watch: {
      peep: {
        files: "*",
        tasks: ["default"]
      }
    }
  });

  // Load plugins
  grunt.loadNpmTasks('grunt-contrib-copy');
  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-contrib-watch');

  // Tasks
  grunt.registerTask('default', ["less", "copy:dev"]);
};
