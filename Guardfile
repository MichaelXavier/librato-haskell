#notification :libnotify

guard :shell do
  watch(%r{.*\.cabal$}) do
    ncmd("cabal build && cabal test")
  end

  def ncmd(cmd, msg = cmd)
    if system(cmd)
      n "#{msg} SUCCEEDED"
    else
      n "#{msg} FAILED"
    end
  end
 
  def run_tests(module_path)
    filename = "test/#{module_path}Spec.hs"

    if File.exists?(filename)
      ncmd("ghc -isrc -itest -e 'hspec spec' #{filename} test/Spec.hs")
    end
  end

  watch(%r{src/(.+)\.hs$}) do |m|
    run_tests(m[1])
  end
 
  watch(%r{test/(.+)Spec\.hs$}) do |m|
    run_tests(m[1])
  end
 
end
