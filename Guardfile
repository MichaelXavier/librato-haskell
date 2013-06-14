guard :shell do
  watch(%r{.*\.cabal$}) do
    `cabal build && cabal test`
  end
 
  def run_tests(module_path)
    filename = "test/#{module_path}Spec.hs"

    if File.exists?(filename)
      `ghc -isrc -itest -e 'hspec spec' #{filename} test/Spec.hs`
    end
  end

  watch(%r{src/(.+)\.hs$}) do |m|
    run_tests(m[1])
  end
 
  watch(%r{test/(.+)Spec\.hs$}) do |m|
    run_tests(m[1])
  end
 
end
