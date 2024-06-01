import { faker } from '@faker-js/faker'
import { Button, notification } from 'antd'
import { ProtectedTemplate } from '../templates/ProtectedTemplate'
import { http } from '../../http'
import { AxiosError } from 'axios'

export function LoremPage() {
  const [notificationApi, contextHolder] = notification.useNotification()

  return (
    <ProtectedTemplate title="Page title">
      {contextHolder}

      <div>
        <p>
          <Button
            type="primary"
            onClick={async () => {
              try {
                const result = await http.post(`/Something/Test`)

                notificationApi.success({
                  message: 'Request OK',
                  description: result.data,
                })
              } catch (e) {
                if (e instanceof AxiosError) {
                  notificationApi.error({
                    message: e.message,
                    description: <code className="text-xs">{JSON.stringify(e.response)}</code>,
                  })
                }
              }
            }}
          >
            Backend test request
          </Button>
        </p>
        <br />
        <p>{faker.lorem.sentence(1000)}</p>
      </div>
    </ProtectedTemplate>
  )
}
