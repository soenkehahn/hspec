require 'digest'
require "jekyll-less"

# use our custom version of pygments
Pygments.start(File.join(File.dirname(__FILE__), "../pygments"))

module Hspec
  module CustomFilters
    def runhaskell(args)
      cmd = "runhaskell -Wall -Werror -i../src -optP-include -optP../dist/build/autogen/cabal_macros.h #{args}"
      cache  = ".cache/runhaskell"
      system "mkdir -p #{cache}"

      digest = Digest::MD5.hexdigest(cmd)
      file   = File.join cache, digest

      puts "#{cmd}"
      if File.exists? file
        puts "  using cache file #{file}"
        File.read file
      else
        r = `#{cmd}`
        File.write file, r
        puts "  created cache file #{file}"
        r
      end
    end

    def id(name)
      haskell_identifiers = {
        'property'          => 'http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/Test-QuickCheck.html#v:property',
        'Property'          => 'http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/Test-QuickCheck.html#t:Property',
        'Testable'          => 'http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/Test-QuickCheck-Property.html#t:Testable',

        '@?='               => 'http://hackage.haskell.org/packages/archive/HUnit/latest/doc/html/Test-HUnit-Base.html#v:-64--63--61-',

        'hspec'             => 'http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec-Runner.html#v:hspec',
        'hspecWith'         => 'http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec-Runner.html#v:hspecWith',
        'fromHUnitTest'     => 'http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec-HUnit.html#v:fromHUnitTest',

        'Selector'          => 'http://hackage.haskell.org/packages/archive/hspec-expectations/latest/doc/html/Test-Hspec-Expectations.html#t:Selector',
        'shouldThrow'       => 'http://hackage.haskell.org/packages/archive/hspec-expectations/latest/doc/html/Test-Hspec-Expectations.html#v:shouldThrow',
        'errorCall'         => 'http://hackage.haskell.org/packages/archive/hspec-expectations/latest/doc/html/Test-Hspec-Expectations.html#v:errorCall',

        'isPermissionError' => 'http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO-Error.html#v:isPermissionError',
        'evaluate'          => 'http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception.html#v:evaluate',
        'ErrorCall'         => 'http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception.html#t:ErrorCall',

        'force'             => 'http://hackage.haskell.org/packages/archive/deepseq/latest/doc/html/Control-DeepSeq.html#v:force',
      }
      url = haskell_identifiers[name]
      if url
        "[`#{name}`](#{url})"
      else
        puts "WARNING: No link destination for #{name}!"
        "`#{name}`"
      end
    end
  end
end

Liquid::Template.register_filter(Hspec::CustomFilters)

module Hspec
  class ExampleTag < Liquid::Tag
    def initialize(tag_name, file, tokens)
      super
      @file = file.strip
    end

    def render(context)
      file = File.join '_includes', @file
      partial = Liquid::Template.parse(add_wrapping file)
      context.stack do
        partial.render(context)
      end
    end

    def add_wrapping(file)
      source = File.read(file)
<<-HTML
{% highlight hspec %}
-- file Spec.hs
#{source}
{% endhighlight %}
<pre><kbd class="shell-input">runhaskell Spec.hs</kbd>
<samp>{{ "#{file} --html" | runhaskell }}</samp></pre>
HTML
    end
  end
end

module Hspec
  class FoldableExampleTag < ExampleTag
    def add_wrapping(*)
      source = super
      # It is crucial to indent nested HTML tags, otherwise a bug in sundowns
      # parser is triggered, which leads to invalid HTML!  See
      # https://github.com/vmg/sundown/issues/139.
<<-HTML
<div>
  <h5 class="foldable">Example code:</h5>
  <div>
#{source}
  </div>
</div>
HTML
    end
  end
end

Liquid::Template.register_tag('inline_example', Hspec::ExampleTag)
Liquid::Template.register_tag('example', Hspec::FoldableExampleTag)
