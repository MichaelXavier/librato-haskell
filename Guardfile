#notification :libnotify

guard :shell do
  watch(%r{.*\.cabal$}) do
    ncmd("cabal build && cabal test")
  end

  def ncmd(cmd, msg = cmd)
    output = `#{cmd}`
    puts output
    summary = output.lines.grep(/examples/).first

    if $?.success?
      n "Build Success!", summary
    else
      n "Failed", summary
    end
  end
 
  def run_tests
    ncmd("ghc -isrc -itest -e 'hspec spec' test/**/*.hs test/Spec.hs")
  end

  watch(%r{src/(.+)\.hs$}) do |m|
    run_tests
  end
 
  watch(%r{test/(.+)Spec\.hs$}) do |m|
    run_tests
  end
 
end
