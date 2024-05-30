import { Layout } from 'antd'
import { ReactNode } from 'react'
import { FooterLove } from './common/FooterLove'

interface PublicTemplateProps {
  children: ReactNode
}

export function PublicTemplate({ children }: PublicTemplateProps) {
  return (
    <Layout className="h-screen">
      <Layout.Content>
        <div className="bg-gray-100 flex flex-col min-h-full justify-center">{children}</div>
      </Layout.Content>

      <Layout.Footer>
        <FooterLove />
      </Layout.Footer>
    </Layout>
  )
}
