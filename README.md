Foray — Fortran Build Tool
=============================================

Author: Drew McCormack<br />
Updated: 2008<br \>

Fortran 90 can include reasonably complex dependencies, which must be taken into account when building a multiple-file program. Unfortunately, most build tools either don't support Fortran, or don't help the developer much. A standard `make` file, for example, requires you to enter dependencies manually, or develop a script to do it for you.

Foray is a build tool designed specifically for Fortran projects. It can be applied to anything from a small utility program with tens of files to a million line monster. Foray natively handles Fortran dependencies, takes care of locating files in subdirectories, and includes advanced features like multi-threading for better performance on multi-core systems, and multiple build configurations (_eg_ debug, release, serial, parallel).

This document will introduce you to Foray, and give instructions for configuring it to work with your Fortran project.

What's the Problem?
-------------------
There are lots of build tools that support C-based languages, but very few that can handle Fortran. And yet, if anything, determining the file dependencies and build order of a Fortran 90 program is more difficult than for a C program. Where dependencies arise in C via source files with `#include` directives, a single Fortran 90 file can include multiple modules, each of which can be 'used' by other files, creating build order dependencies. Add to that that most Fortran compilers generate module files in addition to object files, and you have a reasonably complicated soup to digest.

What Options are Available?
------------------------------
Most Fortran developers stick to `make` for their building needs, but `make` is an old tool, and doesn't have any direct support for Fortran. You either fill in the dependencies by hand, which is error prone, or you use a script to generate the dependencies. This works, but is less than ideal, and there are other reasons not to use `make`, which will be discussed more below.

A more up-to-date option is [SCons](http://www.scons.org/). SCons is an advanced build system, and has support for Fortran, in addition to many other languages. SCons is a very worthy tool, but can be difficult to configure for Fortran. Foray is not designed to be as configurable as Scons; instead, it is an attempt to create an easy-to-use tool specifically for Fortran programs, which includes some of the advanced features of Scons.

Philosophy and Design Requirements
----------------------------------
Here follows a list of basic design requirements that formed the basis of Foray, and some justification for them.

Foray should

*	Not be general purpose.
	*	It should do Fortran well, and only handle enough C to get by. Java -- forget it!
*	Scale to millions of lines, but also be easy to use with small programs.
*	Be very simple to install, preferably just one file.
	*	Don't want to have to have a build tool to build your build tool!
*	Favor convention over configuration.
	*	Foray sacrifices generality for simplicity. Foray chooses a reasonable convention for how projects should be laid out, and will work in any project that is structured in that way.
*	Support multiple, interdependent targets.
	*	Large projects typically have many libraries and executables. Foray needs to handle dependencies between these targets.
*	Support multiple build configurations (*eg*, debug, release, parallel, serial).
*	Work on all Unix/Linux platforms.
	*	Sorry Windows users. We've never tested Foray on Windows, but we assume it doesn't work. May not take much to get it to work though.
*	Scale on multi-core systems.
*	Understand Fortran dependencies, and determine them automatically.
*	Not mix build configuration files (*eg* make files) with source files.
	*	All configuration should be in one file in the project root. We find this preferable to the way `make`, and even `scons`, favor recursive builds with a configuration file in every source directory. It's analogous to how some source control tools (*eg.* CVS and Subversion) write their metadata in directories in the source tree. Our view is that tools should not mix their data directly with the source tree, and this is a philosophy shared by many recently-developed programming tools (_eg_ [Git](http://git.or.cz/)).
*	Have the ability to set different compile options for different groups of files, or individual files.
	*	Fortran compilers have bugs. It is rare that one set of compile flags work for all files in a large program. And often you will want to set higher optimization for certain performance critical files.
*	Consider the compile options used to compile a file when determining if it needs recompiling.
	*	This idea is stolen from SCons (Thanks SCons!). Often you make a change to some compiler flags for a particular subset of source files, and then need to figure out which files need 'touching' so that they get recompiled. Foray stores the compile flags used for each file, and knows when they have changed, automatically rebuilding the file.
*	Separate build products from source code, in a standalone directory.
	*	Some build systems mix object files and other intermediate products through the source tree. Not good. Foray puts all build products in a standalone build directory in the project root.
*	Consider a file modified if its content is modified, as well as its modification date.
	*	Another idea taken from SCons. This can be useful if, say, you move a file aside and temporarily replace it with some other file. When you put it back again, most build systems will not rebuild the file, because the modification date of the source file is not newer than that of the object file. Foray will do a checksum, and see that the file is changed.
*	Use archives in place of object files.
	*	Build systems like `make` compare the modification date of object files to the corresponding source file to determine if a recompile is needed. This is not very robust, and results in object files being spread all over your project. Foray archives object files in static libraries, and stores time stamps in a separate database.
*	Support custom preprocessors.
	*	Many Fortran projects use the C processor, or even custom preprocessors. Foray supports preprocessors by allowing developers to write custom functions (in Python) to preprocess source files before they are compiled.
*	Have a 'live' configuration file.
	*	Foray is written in Python, and you configure it for your project using a Python script called the _BuildInfo_ file. In its simplest form, this involves filling values into a static data structure &mdash; you do not need to have a good grasp of Python. But if you do know how to script in Python, you can add any code you like to the BuildInfo file, and it will be executed before the build starts. Foray includes various 'hooks' to perform actions at different phases of a build.

Installing Foray
------------------
To install Foray, just download it and ensure the `foray` tool is somewhere in your path. You will have to make sure you have Python version 2.4 or later to run it ([Python web site](http://www.python.org)).

Foray Conventions
-------------------
Foray assumes all source code is below a single directory called the *project root*. Each target in the project must exist in one or more subdirectories of the project root directory. Many projects are already structured like this. For example, many projects have a directory called 'src' in the project root that contains all source code. These projects already conform to Foray's convention (provided they only have a single target). Note that there is no restriction on how deep the directory structure of each target goes; Foray will recursively search subdirectories inside a target directory.

Configuring Foray
-------------------
A projects build configuration is usually stored in a file called 'buildinfo' in the project root directory. This file contains all the information used by Foray to build all targets.

The easiest way to get acquainted with a BuildInfo is to take a look at one. Here is an example BuildInfo file.

```python
	buildinfo = \
	{
	    'builddir' : '$FORAY_PROJECT_ROOT/build',
	    'targets' : [
	        {
	            'name' : 'libbase',
	            'rootdirs' : ['base'],
	            'buildsubdir' : 'base',
	            'libraryname' : 'base',
	            'skipdirs' : ['CVS', '.svn'],
	            'skipfiles' : [],
	            'dependson' : [],
	            'compilegroups' : {
	                'safe' : ['HistogramBuilder.f90']
	            }
	        },
	        {
	            'name' : 'cmc',
	            'rootdirs' : ['cmc'],
	            'buildsubdir' : 'cmc',
	            'libraryname' : 'cmc',
	            'skipdirs' : ['CVS', '.svn'],
	            'skipfiles' : [],
	            'dependson' : ['libbase'],
	            'exename' : 'cmc',
	            'mainprogramfile' : 'cmc.f90',
	            'compilegroups' : {
	            }
	        },
	        {
	            'name' : 'cmctests',
	            'rootdirs' : ['cmctests'],
	            'buildsubdir' : 'cmctests',
	            'libraryname' : 'cmctests',
	            'skipdirs' : ['CVS', '.svn'],
	            'skipfiles' : [],
	            'dependson' : ['libbase'],
	            'exename' : 'cmctests',
	            'mainprogramfile' : 'cmctests.f90',
	            'compilegroups' : {
	            }
	        }
	    ],
	    'defaultconfig' : 'debug',
	    'configs' : [
	        {
	            'name' : 'default',
	            'buildsubdir' : 'default',
	            'compileroptions' : {
	                'archivecommand'    : 'ar -r',
	                'unarchivecommand'  : 'ar -d',
	                'ranlibcommand'     : 'ranlib -s',
	                'f77compiler'       : 'gfortran',
	                'f90compiler'       : 'gfortran',
	                'f77flags'          : '-c -m32 -ffixed-form',
	                'f90flags'          : '-c -m32 -ffree-form',
	                'modpathoption'     : '-I',
	                'ccompiler'         : 'gcc',
	                'cflags'            : '-c -m32 -O3 -funroll-loops -malign-natural',
	                'link'              : 'gfortran',
	                'linkflags'         : '',
	                'prioritylibs'      : '',
	                'otherlibs'         : '',
	                'compilegroupflags' : {
	                    'default'       : '',
	                    'safe'          : '-O0'
	                }
	            }
	        },
	        {
	            'name'                  : 'release',
	            'inherits'              : 'default',
	            'buildsubdir'           : 'release',
	            'installdir'            : '$FORAY_PROJECT_ROOT/bin.release',
	            'compileroptions' : {
	                'compilegroupflags' : {
	                    'default'       : '-O3',
	                    'safe'          : '-O2'
	                }
	            }
	        },
	        {
	            'name'                  : 'debug',
	            'inherits'              : 'default',
	            'buildsubdir'           : 'debug',
	            'installdir'            : '$FORAY_PROJECT_ROOT/bin.debug',
	            'compileroptions' : {
	                'f77flags'          : '-c -g -m32 -ffixed-form -fbounds-check',
	                'f90flags'          : '-c -g -m32 -ffree-form -fbounds-check'
	            }
	        }
	    ]
	}
```

This is a standard Python data structure; Foray expects that the variable `buildinfo` will be assigned to this structure. The data structure defines a number of settings needed by Foray:

*	The directory used by Foray to store all intermediate build products (`builddir`).
*	The targets in the project. These can be libraries or executables (`targets`).
*	The build configurations (`configs`), and the default configuration (`defaultconfig`).

Many settings in the BuildInfo file will perform shell expansions. The build directory is a case in point:

```python
	'builddir' : '$FORAY_PROJECT_ROOT/build',
```

This environment variable `$FORAY_PROJECT_ROOT` will be substituted in determining the path to the build directory.

The list corresponding to the `targets` key contains target dictionaries. If the target is a library, it looks like this

```python
	{
	    'name' : 'libbase',
	    'rootdirs' : ['base'],
	    'buildsubdir' : 'base',
	    'libraryname' : 'base',
	    'skipdirs' : ['CVS', '.svn'],
	    'skipfiles' : [],
	    'dependson' : [],
	    'compilegroups' : {
	        'safe' : ['HistogramBuilder.f90']
	    }
	},
```

The dictionary entries are as follows:

*	`name` is the target's name
*	`rootdirs` is a list of subdirectories of the project root that holds the target's source code
*	`buildsubdir` is a subdirectory in the build directory used to store the intermediate build product files of the target;
*	`libraryname` is the name of the library archive used for the target's object files, excluding the 'lib' and '.a' pre- and postfixes
*	`skipdirs` is a list of directory names to skip when scanning for source files
*	`skipfiles` is a list of file names to ignore
*	`dependson` is a list of other targets that must be built before this target gets built
*	`compilegroups` is a dictionary containing lists of files corresponding to special sets of compile flags. In the example above, `safe` is a the name of a compile group, and it contains just the one file `HistogramBuilder.f90`.

An executable target has a few extra keys:

```python
	{
	    'name' : 'cmc',
	    'rootdirs' : ['cmc'],
	    'buildsubdir' : 'cmc',
	    'libraryname' : 'cmc',
	    'skipdirs' : ['CVS', '.svn'],
	    'skipfiles' : [],
	    'dependson' : ['libbase'],
	    'exename' : 'cmc',
	    'mainprogramfile' : 'cmc.f90',
	    'compilegroups' : {
	    }
	},
```

Note that even an executable target has a `libraryname` setting; that's because all object files get archived in static libraries, even for an executable.

The main difference between the library and executable target dictionaries are the `exename` and `mainprogramfile` settings. The `exename` is the name used for the resulting executable, and `mainprogramfile` is the name of the source file that contains the main program. (The object file of the main program will not be archived in the static library.)

Build configurations allow you to build targets with different sets of compile settings. For example, they could be used to build separate parallel, serial, and debug versions of a target. A build configuration looks like this

```python
	{
	    'name' : 'default',
	    'buildsubdir' : 'default',
	    'compileroptions' : {
	        'archivecommand'    : 'ar -r',
	        'unarchivecommand'  : 'ar -d',
	        'ranlibcommand'     : 'ranlib -s',
	        'f77compiler'       : 'gfortran',
	        'f90compiler'       : 'gfortran',
	        'f77flags'          : '-c -m32 -ffixed-form',
	        'f90flags'          : '-c -m32 -ffree-form',
	        'modpathoption'     : '-I',
	        'ccompiler'         : 'gcc',
	        'cflags'            : '-c -m32 -O3 -funroll-loops -malign-natural',
	        'link'              : 'gfortran',
	        'linkflags'         : '',
	        'prioritylibs'      : '',
	        'otherlibs'         : '',
	        'compilegroupflags' : {
	            'default'       : '',
	            'safe'          : '-O0'
	        }
	    }
	},
```

It is useful, though not compulsory, to define standard build settings in one 'default' configuration. This configuration never gets built directly, but is used as the basis of other configurations.

The default configuration above should be fairly self explanatory. It defines fairly standard settings, similar to settings you would see in other build tools. However, there are a couple of settings that could use some additional explanation: `prioritylibs` is a string used in linking to add any external libraries that should appear early in the link command, before any other libraries. Link order can sometimes be significant for resolving symbols, and that is the reason it has been provided. The `compilegroupflags` dictionary defines *extra* compile options that are applied to the files included in the corresponding groups declared earlier in the target settings. One noteworthy point is that all build configurations must have a `default` key in the `compilegroupflags`, which is used for all files that do not fall into any other group.

The `default` configuration above is never actually built, but is used to set default values for other build configurations. This works via an 'inheritance' mechanism, a bit like in object-oriented programming. The `release` build configuration inherits everything from the `default` configuration, and *overrides* a few settings.

```python
	{
	    'name'                  : 'release',
	    'inherits'              : 'default',
	    'buildsubdir'           : 'release',
	    'installdir'            : '$FORAY_PROJECT_ROOT/bin.release',
	    'compileroptions' : {
	        'compilegroupflags' : {
	            'default'       : '-O3',
	            'safe'          : '-O2'
	        }
	    }
	},
```

The `inherits` key gives the name of the inherited configuration. Anything that appears in the *derived* configuration overrides the value from the inherited configuration. This works in a recursive way. For example, the `release` build configuration defines a `compileoptions` dictionary containing one `compilegroupflags` key. This does not mean that all the settings in the `default` configuration's `compileroptions` dictionary will be replaced; only the specific ones provided, like the `default` and `safe` keys in `compilegroupflags` will be replaced. Any others would remain intact.

There is one last aspect of the `release` configuration that demands consideration: the `installdir` setting. After a successful build, Foray will copy any resulting executable to the install directory. You can set the same install directory for each target, in which case only the executable's from the last configuration built will survive afterwards, with all others being overwritten, or you can use a different install directory for each configuration.

Building with Foray
---------------------
Foray is straightforward to use. It must be run from the project root directory. To build all targets, with the default configuration, you can simply issue:

	foray

To build on a multi-core machine, you can set the number of threads using the `-j` option.

	foray -j 2

(You can also set an environment variable to do the same: `FORAY_NUM_THREADS`.)

To only build certain targets, just list them (in any order):

	foray cmctest cmc

Any targets that the listed targets depend upon will also be built.

You can also build multiple configurations at once using the `-b` option.

	foray -b release -b debug cmctest cmc

Each configuration will be build with each target listed.

Finally, there are a few other useful options: `-h` for help; `-d` for verbose debugging output; and `-c` to do a clean build, in which all files are rebuilt.

Advanced Usage
--------------
The example above shows how a standard Fortran project can be configured for building with Foray. Unfortunately, many projects are more complex. Foray offers a few extra configuration options to address cases where the basic configuration is not enough.

#### Using a Preprocessor
To use a preprocessor for your Fortran source code, you need to be a bit familiar with Python, so that you can supply functions that run the preprocessor. The `cmc` example supplied with Foray shows how.

```python
	# Preprocessing functions
	def preprocessedfilename(infile):
	    """
	    Returns the preprocessed file name for the source file name
	    passed in.
	    """
	    base, ext = os.path.splitext(infile)

	    if ext == '.d':        
	        outfile = base + '.f'
	    elif ext == '.d90':
	        outfile = base + '.f90'
	    else:
	        outfile = infile

	    return outfile

	def preprocess(srcPath, outDir):
	    """
	    This function demonstrates how you can preprocess source files.
	    If you do not need a preprocessor, you can remove this function, and
	    the preprocessor related keys from the buildinfo dictionary.
	    This example assumes you want to use the C preprocessor for fortran files.
	    It expects a source file with extension d or d90, and produces a fortran file
	    on output.
	    """
	    filename = os.path.basename(srcPath)
	    outFile = preprocessedfilename(filename)
	    outPath = os.path.join(outDir, outFile)
	    return (0 == subprocess.call('gcc -E -x c -P -C "%s" > "%s"' % (srcPath, outPath), shell=True))


	# Fortran file types
	fortranfiles = \
	{
	    'freeformregex'             : '.*\.f90$',
	    'fixedformregex'            : '.*\.f$',
	    'freeformpreprocessregex'   : '.*\.d90$',
	    'fixedformpreprocessregex'  : '.*\.d$',
	    'preprocessednamefunc'      : preprocessedfilename,
	    'preprocessfunc'            : preprocess,
	    'includefileregex'          : '.*\.fh$',
            'f90defaultCompileGroup'    : 'f90Default',
            'f77defaultCompileGroup'    : 'f77Default'
	}

	...

	# Combine everything in buildinfo dictionary   
	buildinfo = \
	{
	    'builddir'      : '$FORAY_PROJECT_ROOT/build',
	    'fortranfiles'  : fortranfiles,
	    'targets'       : targets,
	    'defaultconfig' : 'debug',
	    'configs'       : configs
	}
```

You need to write two Python functions in the BuildInfo file. The first takes the name of a yet to be preprocessed file, and returns the name of the file produced by the preprocessor. In other words, this function simply translates a file name prior to preprocessing into the file name after preprocessing.

The second function actually performs the preprocessing. In the example above, the `gcc` C preprocessor is used. The function takes the path to the source file, and the path to the directory where the preprocessed file should end up, and returns `True` if preprocessing was successful, and `False` otherwise. The example above shows that in the most common cases, the function should simply run a command that invokes the preprocessor, and ensure that the output file ends up in the directory passed to the function.

That explains how the functions should be written, but not how they are passed to the Foray tool. The functions need to be added to the `buildinfo` data structure, just like all of the other configuration options. To do that, you need to write a entry called `fortranfiles`. The value of this option should be a Python dictionary, which contains the two preprocessing functions, and other entries containing regular expressions that match the file names of the various source file types, as follows

*	`freeformregex`: Should match a standard free-format Fortran file that does not need preprocessing.
*	`fixedformregex`: Should match a standard fixed-format Fortran file that does not need preprocessing.
*	`freeformpreprocessregex`: Should match a free-format Fortran file that needs preprocessing.
*	`fixedformpreprocessregex`: Should match a fixed-format Fortran file that needs preprocessing.
*	`includefileregex`: Should match a Fortran file that gets included in other files, but does not need to be compiled.
*	`preprocessednamefunc`: The function that translates a file name before preprocessing into the file name after preprocessing.
*	`preprocessfunc`: The Python function that invokes the preprocessor.
*       `f90/f77defaultCompileGroup`: Optional. Define a default compile group for f90 / f77 files respectively, which can be defined in compilegroupflags entry of the build config. There default value is set to 'default'.

There is a block analogous to `fortranfiles` for C files, called `cfiles`. Note that if you are only using the standard C preprocessor, you do not need a `cfiles` block. You only need to add a `cfiles` block if you want to run a second preprocessor in addition to the standard C preprocessor. The entries in `cfiles` are

*	`fileNameRegEx`: Should match a standard C file name.
*	`includeFileRegEx`: Should match a standard C header file name.
*	`preprocessFileNameRegEx`: Should match a C file that needs to be preprocessed (by the secondary preprocessor).
*	`preprocessednamefunc`: The function that translates a file name before preprocessing into the file name after preprocessing.
*	`preprocessfunc`: The Python function that invokes the preprocessor.
*       `defaultCompileGroup`: Optional compile group name to be defined in the configs in the compilegroupflags entry. Default value is 'default'.

#### Performing Setup on First Build
In some projects, there may need to be some setup of the build environment on the first build. For example, perhaps certain files are only available in object form, and you thus need to ensure that some of the libraries are populated with these objects before building begins.

Foray provides a hook for this, in the form of a callback function. If you want to perform some setup the first time a particular target and configuration are built, you need to supply a Python function in the buildinfo dictionary, with the key `firstbuildfunc`. The function should take the following arguments:

*	The project root directory.
*	The root source directories of the current target.
*	The directory used to store intermediate build products (_eg_ preprocessed files, module files).
*	The directory used to store libraries for the current target/config combination.
*	The directory used to store executables for the current target/config combination.
*	The install directory for the current target/config combination.

You can use any of these arguments to aid your setup.

#### Preparing to Build a Particular Configuration
Foray also provides a hook for preparing your project to build a particular build configuration. Perhaps certain files need to be swapped in or out when building a particular configuration.

The function should be supplied in the BuildInfo file with the key `prepareconfigfunc`. It should take one argument, the name of the build configuration. If you need access to other aspects of the build environment, you can access the Foray environment variables (see below).

#### Environment Variables
Foray maintains a set of environment variables while it builds.  These can be accessed in your scripts or BuildInfo file. Here is a list of Foray's environment variables:

*	`FORAY_PROJECT_ROOT`: The project root directory.
*	`FORAY_TARGET_ROOT_DIRS`: The source directories of the current target.
*	`FORAY_INTERMEDIATE_PRODUCTS_DIR`: The directory used to store intermediate build products (_eg_ preprocessed source)
*	`FORAY_LIBRARY_PRODUCTS_DIR`: The directory used to store product libraries.
*	`FORAY_EXECUTABLE_PRODUCTS_DIR`: The directory used to store product executables.
*	`FORAY_INSTALL_DIR`: The install directory for the current build config.

#### Debugging Your BuildInfo File
You can get verbose debugging output from the `foray` tool by supplying the `-d` option. You can use the same mechanism to debug your BuildInfo file: Simply import the Python `logging` module, and add logging statements to BuildInfo, like this

```python
	    logging.debug('This is a debugging string')
```

Your debugging messages will only appear when the `-d` option is supplied to Foray.

Foray's License
---------------
Foray is available under the New BSD license, so do with it as you please, as long as you don't claim it to be your own. (The license is at the bottom of the `foray` script.) The latest version of Foray is available at Google Code.

Known Issues
------------
The following issues will be addressed in future releases:
*	Foray will not yet handle C dependencies. In other words, if you change a C header file, it will not see that files including that header need to be recompiled. For the time being, if you make changes in C headers, you need to touch any files using them.
*	Dependencies arising from Fortran 'include' statements are not yet handled.
*	It is not yet possible to tell Foray that certain preprocessor statements lead to dependencies. For example, if you use the C preprocessor with your Fortran source files, and have `#include` directives, Foray will not recognize the implied dependency.
