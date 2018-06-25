-- import da biblioteca
import Graphics.UI.Gtk

--minha função

main :: IO()
main = do
		 -- função chamada por todos aplicações gtk2hs
		 initGUI
		 -- as proximas linhas cria bloco e mostra uma
		 -- janela e seu conteudo (widgets)
		 window <- windowNew
		 widgetShowAll window

		 -- a linha abaico é o main event loop da app
		 mainGUI