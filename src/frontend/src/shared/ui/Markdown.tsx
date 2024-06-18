import { Typography, List } from 'antd'
import remarkGfm from 'remark-gfm'
import rehypeRaw from 'rehype-raw'
import ReactMarkdown from 'react-markdown'

interface MarkdownProps {
  markdown: string
}
export function Markdown({ markdown }: MarkdownProps) {
  markdown = markdown.replace(/(<br>)/g, '\n')

  return (
    <ReactMarkdown
      className="markdown-renderer"
      components={{
        h1: props => <Typography.Title level={1}>{props.children}</Typography.Title>,
        h2: props => <Typography.Title level={2}>{props.children}</Typography.Title>,
        h3: props => <Typography.Title level={3}>{props.children}</Typography.Title>,
        h4: props => <Typography.Title level={4}>{props.children}</Typography.Title>,
        h5: props => <Typography.Title level={5}>{props.children}</Typography.Title>,
        li: props => <List.Item>{props.children}</List.Item>,
        ol: props => (
          <ol
            {...props}
            className="markdown-renderer-ol"
          />
        ),
        p: props => <Typography.Paragraph>{props.children}</Typography.Paragraph>,
        em: props => <Typography.Text italic>{props.children}</Typography.Text>,
        strong: props => <Typography.Text strong>{props.children}</Typography.Text>,
        blockquote: props => <Typography.Text type="secondary">{props.children}</Typography.Text>,
        img: props => (
          <img
            {...props}
            src={'/' + props.src}
          />
        ),
      }}
      remarkPlugins={[remarkGfm]}
      rehypePlugins={[rehypeRaw]}
    >
      {markdown}
    </ReactMarkdown>
  )
}
