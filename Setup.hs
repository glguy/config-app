import Distribution.MacOSX
import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
       }

guiApps :: [MacApp]
guiApps = [MacApp "config-app-wkwebview"
                  Nothing
                  (Just "macos/Info.plist")
                  ["index.html", "prism.css", "prism.js"]
                  [] -- No other binaries.
                  DoNotChase -- Try changing to ChaseWithDefaults
          ]
