if exists('g:vscode')

    noremap <Leader>cs :call VSCodeNotify('clover.connectSocketRepl')<CR>  
    noremap <Leader>c0 :call VSCodeNotify('clover.clear-console')<CR>  
    noremap <Leader>eF :call VSCodeNotify('clover.evaluate-top-block')<CR>  
    noremap <Leader>ef :call VSCodeNotify('clover.evaluate-block')<CR>  
    vnoremap <Leader>ef :call VSCodeNotify('clover.evaluate-selection')<CR>  

    noremap <Leader>et :call VSCodeNotify('workbench.action.tasks.runTask', 'Clover: Tap top block')<CR>
    noremap <Leader>eT :call VSCodeNotify('workbench.action.tasks.runTask', 'Clover: Tap block')<CR>  
    noremap <Leader>es :call VSCodeNotify('workbench.action.tasks.runTask', 'Clover: Tap selection')<CR>  
    noremap <Leader>ed :call VSCodeNotify('workbench.action.tasks.runTask', 'Clover: Tap def var')<CR>  
    noremap <Leader>ev :call VSCodeNotify('workbench.action.tasks.runTask', 'Clover: Tap var')<CR>  
    noremap <Leader>en :call VSCodeNotify('workbench.action.tasks.runTask', 'Clover: Tap ns')<CR>  
    noremap <Leader>rn :call VSCodeNotify('workbench.action.tasks.runTask', 'Clover: Tap remove ns')<CR>  
    noremap <Leader>eR :call VSCodeNotify('workbench.action.tasks.runTask', 'Clover: Tap reload all ns')<CR>  
    noremap <Leader>rt :call VSCodeNotify('workbench.action.tasks.runTask', 'Clover: Tap run current test')<CR>  
    noremap <Leader>rT :call VSCodeNotify('workbench.action.tasks.runTask', 'Clover: Tap run tests')<CR>  
    noremap <Leader>rst :call VSCodeNotify('workbench.action.tasks.runTask', 'Clover: Tap run side tests')<CR>  
    noremap <Leader>hd :call VSCodeNotify('workbench.action.tasks.runTask', 'Clover: Tap doc var')<CR>  
    noremap <Leader>hj :call VSCodeNotify('workbench.action.tasks.runTask', 'Clover: Tap javadoc')<CR>  
    noremap <Leader>al :call VSCodeNotify('workbench.action.tasks.runTask', 'Clover: Tap add libs')<CR>  
endif