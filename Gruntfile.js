module.exports = function(grunt) {
  const DEV_PATH = "/home/foundry/foundrydata/Data/systems/mythic/";
  const RELEASE_DIR = "/home/tab/Documents";
  const RELEASE_PATH = RELEASE_DIR + "/mythic";
  const RELEASE_TASKS = ["less", "copy:release", "uglify", "compress", "clean"];

  // Configuration
  grunt.initConfig({
    // grunt-contrib-clean
    clean: {
      options: { force: true },
      src: [ RELEASE_PATH ]
    },

    // grunt-contrib-compress
    compress: {
      main: {
        options: {
          archive: RELEASE_DIR + "/mythic.zip",
          pretty: true
        },
        files: [{
          expand: true,
          cwd: RELEASE_PATH,
          src: "**/*",
          dest: "/"
        }]
      }
    },

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
      },
      release: {
        files: [
          {expand: true, src: "lang/*",        dest: RELEASE_PATH},
          {expand: true, src: "templates/**",  dest: RELEASE_PATH},
          {expand: true, src: "CHANGELOG.md",  dest: RELEASE_PATH},
          {expand: true, src: "LICENSE.txt",   dest: RELEASE_PATH},
          {expand: true, src: "mythic.css",    dest: RELEASE_PATH},
          {expand: true, src: "README.md",     dest: RELEASE_PATH},
          {expand: true, src: "system.json",   dest: RELEASE_PATH},
          {expand: true, src: "template.json", dest: RELEASE_PATH}
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

    // grunt-contrib-uglify
    uglify: {
      main: {
        files: [
          {expand: true, src: "module/**/*.js", dest: RELEASE_PATH},
          {expand: true, src: "mythic.js",      dest: RELEASE_PATH}
        ]
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
  grunt.loadNpmTasks('grunt-contrib-clean');
  grunt.loadNpmTasks('grunt-contrib-compress');
  grunt.loadNpmTasks('grunt-contrib-copy');
  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-contrib-watch');

  // Tasks
  grunt.registerTask('default', ["less", "copy:dev"]);
  grunt.registerTask('release', RELEASE_TASKS);
};
