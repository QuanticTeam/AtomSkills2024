import { z } from 'zod'
import { LockOutlined, UserOutlined } from '@ant-design/icons'
import { Alert, Button, Checkbox, Form, Input, Select, Space, Typography } from 'antd'
import { isStrongPassword } from 'validator'
import { Logo } from '../common/Logo'
import { PublicTemplate } from '../templates/PublicTemplate'
import { Link } from 'react-router-dom'

function getIsPasswordStrong(value: string, returnScore = false): boolean | number {
  return isStrongPassword(value, {
    minLength: 8,
    minLowercase: 1,
    minUppercase: 0,
    minNumbers: 0,
    minSymbols: 0,
    returnScore: returnScore as any,
    pointsPerUnique: 1,
    pointsPerRepeat: 0.5,
    pointsForContainingLower: 10,
    pointsForContainingUpper: 10,
    pointsForContainingNumber: 10,
    pointsForContainingSymbol: 10,
  })
}

const RegisterData = z.object({
  username: z
    .string()
    .trim()
    .min(1, 'Обязательно заполните это поле')
    .min(2)
    .max(56, 'Слишком длинное имя пользователя'),
  password: z
    .string()
    .trim()
    .min(1, 'Обязательно заполните это поле')
    .min(8, 'Слишком короткий пароль')
    .max(32, 'Слишком длинный пароль')
    .regex(/^[A-Za-z0-9-_+!?=#$%&@^`~]+$/, 'Недопустимые символы')
    .refine(p => !getIsPasswordStrong(p), 'Пароль слишком слабый'),
  remember: z.boolean(),
})

// console.log(RegisterData.parse({ username: 'Ludwig', password: '~а~~~~~~~', remember: false }))

type RegisterData = z.infer<typeof RegisterData>

export function RegisterPage() {
  return (
    <PublicTemplate>
      <div className="w-72 flex mx-auto mb-16">
        <Logo full />
      </div>

      <div className="flex">
        <Form
          initialValues={{ remember: true }}
          onFinish={() => {}}
          className="w-4/5 m-auto"
          labelCol={{ span: 3, offset: 5 }}
          wrapperCol={{ span: 8 }}
          layout="horizontal"
        >
          <Form.Item
            required
            label="Ваша роль"
          >
            <Select
              options={[
                {
                  value: 'participant',
                  label: 'Участник',
                },
                {
                  value: 'judge',
                  label: 'Судья',
                },
              ]}
              placeholder="Выбрать"
            />
          </Form.Item>
          <Form.Item
            name="username"
            rules={[{ required: true, message: 'Please input your Username!' }]}
            label="Пользователь"
          >
            <Space
              direction="vertical"
              className="w-full"
            >
              <Alert
                showIcon
                message={
                  <Typography.Text className="text-xs text-slate-500">
                    <Space direction="vertical">
                      <div>Латинские буквы и цифры от 2 до 56 символов</div>
                    </Space>
                  </Typography.Text>
                }
              />
              <Input
                prefix={<UserOutlined className="text-slate-400" />}
                placeholder="Придумайте логин"
              />
            </Space>
          </Form.Item>
          <Form.Item
            name="password"
            rules={[{ required: true, message: 'Please input your Password!' }]}
            required
            label="Пароль"
          >
            <Space
              direction="vertical"
              className="w-full"
            >
              <Alert
                showIcon
                message={
                  <Typography.Text className="text-xs text-slate-500">
                    <Space direction="vertical">
                      <div>
                        Латинские буквы, цифры,{' '}
                        <code
                          className="font-bold text-slate-700"
                          title="- _ + ! ? = # $ % & @ ^ ` ~"
                        >
                          - _ + ! ? = # $ % & @ ^ ` ~
                        </code>{' '}
                        от 8 до 32 символов
                      </div>
                    </Space>
                  </Typography.Text>
                }
              />
              <Input
                prefix={<LockOutlined className="text-slate-400" />}
                type="password"
                placeholder="Придумайте пароль"
                suffix={
                  <Typography.Text className="text-xs text-slate-400">
                    {(() => {
                      return 'средний'
                      // const score = getIsPasswordStrong('111111111!', true) as number
                      // if (score < 25) return 'слабый' + score
                      // if (score >= 25 && score < 35) return 'средний' + score
                      // if (score >= 40) return 'надежный' + score
                    })()}
                  </Typography.Text>
                }
              />
            </Space>
          </Form.Item>

          <Form.Item
            label=" "
            colon={false}
          >
            <Space
              className="w-full"
              direction="vertical"
              classNames={{ item: 'flex justify-center' }}
            >
              <Button
                type="primary"
                htmlType="submit"
                className="w-full"
              >
                Зарегистрироваться
              </Button>
              <Typography.Text>
                или <Link to="/login">войти</Link>
              </Typography.Text>
            </Space>
          </Form.Item>
        </Form>
      </div>
    </PublicTemplate>
  )
}
