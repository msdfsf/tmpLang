import { createHighlighter } from 'shiki';
import { readFileSync, writeFileSync } from 'node:fs';
import { JSDOM } from 'jsdom';

async function highlightCode() {
    let myCustomTheme;
    let grammar;
    let hg;

    try {
        console.log('Loading custom theme...');
        myCustomTheme = JSON.parse(readFileSync('./Witchesbrew.tmTheme.json', 'utf-8'));
        console.log('Theme loaded.');
    } catch (error) {
        console.error('Error loading or parsing theme file:', error);
        process.exit(1);
    }

    try {
        console.log('Loading grammar...');
        grammar = JSON.parse(readFileSync('../tools/VS-Code plugins/language-support/syntaxes/vi.tmLanguage.json', 'utf8'));
        console.log('Grammar loaded.');
    } catch (error) {
        console.error('Error loading or parsing grammar file:', error);
        process.exit(1);
    }

    try {
        console.log('Creating highlighter...');
        hg = await createHighlighter({
            langs: [grammar],
            themes: [myCustomTheme, 'nord', 'dracula-soft']
        });
        console.log('Highlighter created.');
    } catch (error) {
        console.error('Error creating Shiki highlighter:', error);
        process.exit(1);
    }

    let document;
    try {
        console.log('Loading HTML document...');
        document = new JSDOM(readFileSync('doc_tmp.html', 'utf-8')).window.document;
        console.log('HTML document loaded.');
    } catch (error) {
        console.error('Error loading or parsing HTML file:', error);
        process.exit(1);
    }

    const blocks = document.querySelectorAll('pre');
    
    blocks.forEach((block, index) => {
        
        const codeToHighlight = block.textContent?.trim() ?? '';
        if (!codeToHighlight) {
            console.warn(`Block ${index + 1} is empty, skipping.`);
            return;
        }

        try {

            const themeName = myCustomTheme.name || 'Witchesbrew';
            let hgStr = hg.codeToHtml(codeToHighlight, { lang: 'vi', theme: themeName });

            // Replace original <pre> with Shiki's <pre>
            let tempDiv = document.createElement('div');
            tempDiv.innerHTML = hgStr; // Shiki returns a full <pre>...</pre> string
            let shikiPre = tempDiv.firstChild;

            if (shikiPre && shikiPre.nodeName === 'PRE') {
                block.replaceWith(shikiPre);
            } else {
                 console.warn(`Block ${index + 1}: Shiki did not return a valid <pre> element. HTML: ${hgStr}`);
            }

        } catch (error) {
            console.error(`Error highlighting block ${index + 1}:`);
            console.error("Original content:", `"${codeToHighlight}"`);
            console.error("Error details:", error);
        }
    });

    try {
        console.log('Writing output to doc.html...');
        writeFileSync('doc.html', document.documentElement.outerHTML);
        console.log('Finished writing doc.html.');
    } catch (error) {
        console.error('Error writing output HTML file:', error);
        process.exit(1);
    }
}

highlightCode().catch(error => {
    console.error("Unhandled error during script execution:", error);
    process.exit(1);
});
